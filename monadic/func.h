#ifndef FUNC_H
#define FUNC_H

#define then_state      <<state::bind>>
#define then_maybe      <<maybe::bind>>
#define then_either     <<either::bind>>
#define then_list       <<list::bind>>
#define then_writer     <<writer::bind>>
#define then_reader     <<reader::bind>>

namespace functional{

    //infix notation
    /*-----------------------------------------------------------*/
    template <typename left, typename op>
    struct Left
    {
        const left& left_operand;
        const op& operation;
        Left(const left& left_operand,
                   const op& operation)
            : left_operand(left_operand), operation(operation) {}
    };

    template <typename left, typename op >
    auto operator << (const left& left_operand,
                     op& operation)
    {
        return Left<left, op>(left_operand, operation);
    }

    template <typename left, typename op, typename right>
    auto operator >> (Left<left, op> leftStruct,
                     const right& right_operand)
    {
        return leftStruct.operation(leftStruct.left_operand, right_operand);
    }

    //tuple
    /*-----------------------------------------------------------*/

    namespace tuple{
        auto pair = [] (auto _1) {
            return [_1] (auto _2) {
                return [_1,_2] (auto accessor){
                    return accessor(_1)(_2);
                };
            };
        };
        auto first = [] (auto p) {
            return p([] (auto _1){
                return [_1] (auto){
                    return _1;
                };
            });
        };
        auto second = [] (auto p) {
            return p([] (auto){
                return [] (auto _2){
                    return _2;
                };
            });
        };
    }

    //monads
    /*-----------------------------------------------------------*/
    namespace state{
        auto mreturn = [] (auto val){
            return [val] (auto state){
                return tuple::pair(val)(state);
            };
        };

        auto runState = [] (auto sa, auto init_state){
            return sa(init_state);
        };

        auto bind = [] (auto sa, auto f){
            return [sa, f] (auto state){
                auto temp = runState(sa, state);
                auto new_state = f(tuple::first(temp));
                return runState(new_state, tuple::second(temp));
            };
        };

        auto get = [] (auto state){
            return tuple::pair(state)(state);
        };
        auto put = [] (auto x) {
            return [x] (auto){
                return tuple::pair(nullptr)(x);
            };
        };

        auto evalState = [] (auto sa, auto f){
            return tuple::first(runState(sa, f));
        };

        auto execState = [] (auto sa, auto f){
            return tuple::second(runState(sa, f));
        };
    }

    namespace maybe{

        auto mreturn = [] (auto val){
            return tuple::pair("just")(val);
        };

        auto bind = [] (auto ma, auto f) {
            return tuple::first(ma) == "just"
                    ? f(tuple::second(ma))
                    : tuple::pair("nothing")(0);
        };
    }

    namespace either{
        auto mreturn = [] (auto val){
            return tuple::pair("left")(tuple::pair(val)(""));
        };

        auto bind = [] (auto ea, auto f){
            return tuple::first(ea) == "left"
                    ? f(tuple::first(tuple::second(ea)))
                    : tuple::pair("right")(tuple::pair(tuple::first(tuple::second(ea)))(tuple::second(tuple::second(ea))));
        };
    }

    namespace writer{

        auto mreturn = [] (auto val){
            return tuple::pair(val)(std::string(""));
        };
        auto bind = [] (auto wa, auto f){
            auto newWriter = f(tuple::first(wa));
            return tuple::pair(tuple::first(newWriter))(tuple::second(wa) + std::string("\n") + tuple::second(newWriter));
        };
    }

    namespace list{

        template<class none = void>
        auto make_list()
        {
            return nullptr;
        }

        template<class Type, Type First, Type... Rest>
        auto make_list()
        {
            return tuple::pair(First)(make_list<Type, Rest...>());
        }

        auto head = tuple::first;
        auto tail = tuple::second;

        auto cons = [] (auto x, auto xs) {
            return tuple::pair(x)(xs);
        };

        template<class List, class F>
        auto map(List lst, F f){
            auto result = f(head(lst));
            return tuple::pair(result)(map(tail(lst), f));
        }
        template<class F>
        auto map(std::nullptr_t, F){
            return nullptr;
        }

        template<class List, class F, class Init>
        auto foldLeft(List lst, F f, Init init){
            return foldLeft(tail(lst), f, f(init, head(lst)));
        }
        template <class F, class Init>
        auto foldLeft(std::nullptr_t, F, Init init){
            return init;
        }

        template<class List, class F, class Init>
        auto foldRight(List lst, F f, Init init){
            return f(head(lst), foldRight(tail(lst), f, init));
        }
        template <class F, class Init>
        auto foldRight(std::nullptr_t, F, Init init){
            return init;
        }

        auto show = [] (auto lst){
            auto _show = [] (auto lst) {
                map(lst, [] (auto elem) {std::cout<<elem<<" "; return "";});
            };
            _show(lst);
            std::cout<<"nil"<<std::endl;
        };

        auto append = [] (auto lst_a, auto lst_b){
            return foldRight(lst_a, [] (auto elem, auto acc) {
                return elem <<cons>> acc;
            }, lst_b);
        };


        auto concat = [] (auto lst){
            return foldLeft(lst, append , nullptr);
        };

        auto mreturn = [] (auto x) {
            return tuple::pair(x)(nullptr);
        };

        auto bind = [] (auto sa, auto f){
            return concat(map(sa, f));
        };
    }

    namespace reader{

        auto runReader = [] (auto ra, auto init_env) {
            return ra(init_env);
        };

        auto mreturn = [] (auto val) {
            return [val] (auto /*env*/) {
                return val;
            };
        };

        auto bind = [] (auto ra, auto f){
            return [ra, f] (auto env) {
                return runReader(f(runReader(ra, env)), env);
            };
        };

        auto ask = [] (auto env) {return env;};

        auto asks = [] (auto f){
            return ask <<bind>> [f] (auto env) {return mreturn(f(env));};
        };
    }

    namespace cont{

        auto runCont = [] (auto ca, auto cont){
            return ca(cont);
        };
        auto mreturn = [] (auto val){
            return [val] (auto cont) {
                return cont(val);
            };
        };

        auto bind = [] (auto ca, auto f) {
            return [ca, f] (auto cont) {
                //\k -> c (\a -> runCont (f a) k)
                return ca([f, cont] (auto x) {return runCont(f(x), cont);});
            };
        };

        //callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k
        auto callCC = [] (auto f){
            return [f] (auto cont){
                return runCont(f([cont] (auto x) {return [cont, x] (auto) {return cont(x);};}), cont);
            };
        };
    }
}

#endif // FUNC_H
