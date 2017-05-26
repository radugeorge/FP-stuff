#include <QCoreApplication>
#include <iostream>

#include "func.h"

using namespace functional;

namespace Stack{

    //pop =     get
    //          >>= \(x:xs) -> put xs
    //          >>= \_ -> return x

    auto pop = state::get
               then_state [] (auto state) {auto pop_result = list::head(state); return state::put(list::tail(state))
               then_state [pop_result] (auto) {return state::mreturn(pop_result);};};

    //push x =  get
    //          >>= \xs -> put $ x : xs

    auto push = [] (auto x) {
        return state::get
               then_state [x] (auto state) {return state::put(x <<list::cons>> state);};
    };

    //top =     get
    //          >>= \st@(x:_) -> put st
    //          >>= \_ -> return x

    auto top = state::get
               then_state [] (auto state) {auto top_result = list::head(state); return state::put(state)
               then_state [top_result] (auto) {return state::mreturn(top_result);};};

    auto stackManip =
            push(1)
            then_state [] (auto) {return push(2)
            then_state [] (auto) {return push(3)
            then_state [] (auto) {return push(4)
            then_state [] (auto) {return push(5)
            then_state [] (auto) {return pop
            then_state [] (auto) {return pop
            then_state [] (auto) {return top;};};};};};};};

    auto addFirst2 =
            pop
            then_state [] (auto x) {return pop
            then_state [x] (auto y) {auto sum = x+y; return push(sum)
            then_state [sum] (auto) {return state::mreturn(sum);};};};

    auto manipAndThenAddFirst2 =
            stackManip
            then_state [] (auto) {return addFirst2;};


    auto testStack = [] () {
        auto lst = list::make_list<int>();
        auto result = state::evalState(manipAndThenAddFirst2, lst);
        auto state = state::execState(manipAndThenAddFirst2, lst);

        //result is (<result>, <state>)
        std::cout<<"Stack test - state: ";
        list::show(state);
        std::cout<<"Stack test - result: "<<result<<std::endl;

    };
}

namespace Accumulate{

    auto add = [] (auto x) {
        return state::get
               then_state [x] (auto sum) {return state::put(sum+x);};
    };

    auto substract = [] (auto x){
        return state::get
               then_state [x] (auto sum) {return state::put(sum-x);};
    };

    auto half =
            state::get
            then_state [] (auto sum) {return state::mreturn(sum/2.0);};

    auto accOperations =
        add(3)
        then_state [] (auto) {return add(4)
        then_state [] (auto) {return substract(2)
        then_state [] (auto) {return half;};};};

    auto testAccumulate = [] () {
        auto result = state::evalState(accOperations, 0);
        auto state = state::execState(accOperations, 0);

        std::cout<<"Accumulate test - state: "<<state<<std::endl;
        std::cout<<"Accumulate test - result: "<<result<<std::endl;
    };
}

namespace failingComputations{ //propagates failure status

    auto failingOp = [] (auto){
        return tuple::pair("nothing")(0);
    };

    auto increment = [] (auto x){
        return tuple::pair("just")(x+1);
    };

    auto testFailingComputations = [] () {
        auto invalid = maybe::mreturn(4)
                        then_maybe failingOp
                        then_maybe increment;

        auto valid = maybe::mreturn(4)
                        then_maybe increment
                        then_maybe increment;


        std::cout<<"Invalid operation: "<<tuple::first(invalid)<<" "<<tuple::second(invalid)<<std::endl;
        std::cout<<"Valid operation: "<<tuple::first(valid)<<" "<<tuple::second(valid)<<std::endl;
    };
}

namespace errorComputations{ //propagates error

    auto failingOp = [] (auto){
        return tuple::pair("right")(tuple::pair(0)("Error calling failingOp()"));
    };

    auto increment = [] (auto x){
        return tuple::pair("left")(tuple::pair(x+1)(""));
    };

    auto testErrorComputations = [] () {
        auto invalid = either::mreturn(4)
                        then_either failingOp
                        then_either increment;

        auto valid = either::mreturn(4)
                        then_either increment
                        then_either increment;


        std::cout<<"Invalid operation: "<<tuple::first(invalid)<<" "<<tuple::first(tuple::second(invalid))<<" "<<tuple::second(tuple::second(invalid))<<std::endl;
        std::cout<<"Valid operation: "<<tuple::first(valid)<<" "<<tuple::first(tuple::second(valid))<<" "<<tuple::second(tuple::second(valid))<<std::endl;
    };
}

namespace listMonad{

    auto lst = list::make_list<int, 1,2,3>();
    auto inc_lst = list::map(lst, [] (auto x) {return x+1;});

    auto every2Elements = [] (auto lst_a, auto lst_b){
        return  lst_a
                then_list [lst_b] (auto x) {return lst_b
                then_list [x] (auto y) {return list::mreturn(tuple::pair(x)(y));};};
    };

    auto everySquare = [] (auto lst) {
        return  lst
                then_list [lst] (auto x) {return lst
                then_list [x] (auto y) {return y*y == x ? list::mreturn(x) : list::mreturn(0);};};
    };

    auto testListMonad = [] () {
        std::cout<<"List monad - Test list: ";
        list::show(lst);
        std::cout<<"List monad - Test list: ";
        list::show(inc_lst);
        std::cout<<"List monad - Append test: ";
        list::show(list::append(lst, inc_lst));

        std::cout<<"List monad - every 2 elements from list: \n";
        auto res = every2Elements(list::make_list<int, 1,2,3>(), list::make_list<int, 4,5,6>());
        list::map(res, [] (auto elem) {
            std::cout<<"("<<tuple::first(elem)<<" "<<tuple::second(elem)<<")"<<"\n"; return "";
        });
        std::cout<<std::endl;

        std::cout<<"List monad - every square number below 10: ";
        auto list10 = list::make_list<int,  1,2,3,4,5,6,7,8,9,10>();
        auto everysq = everySquare(list10);
        list::map(everysq, [] (auto e) {
            e == 0
                ? std::cout<<""
                : std::cout<<e<<" ";
            return e;});
        std::cout<<std::endl;
    };
}

namespace writerMonad{
    auto add = [] (auto x){
        return [x] (auto y) {
            return tuple::pair(x+y)("1 - add called");
        };
    };

    auto substract = [] (auto x){
        return [x] (auto y) {
            return tuple::pair(y-x)("2 - substract called");
        };
    };

    auto finishComputation = [] (auto x){
        return tuple::pair(x)("3 - Computation finished");
    };

    auto testLoggingComputations = [] () {
        auto logComp = writer::mreturn(10)
                then_writer add(3)
                then_writer substract(2)
                then_writer finishComputation;

        std::cout<<"Writer monad: result "<<tuple::first(logComp)<<std::endl;
        std::cout<<"Writer monad: log "<<tuple::second(logComp)<<std::endl;
    };
}

namespace readerMonad{

    auto greet =
            reader::ask
            then_reader [] (auto who) {return reader::mreturn("Hello, " + who);};

    typedef enum{
        CFG_0 = 0,
        CFG_1,
        CFG_2
    }ConfigTable;

    auto logWhichCfg =
            reader::ask
            then_reader [] (auto cfg) {std::cout<<"Used config: "<<(int)cfg<<std::endl; return reader::mreturn(cfg);};

    auto doSomethingForCFG_1 =
            reader::asks([] (auto cfg) {return cfg == CFG_1;})
            then_reader [] (auto isCFG_1) {return isCFG_1 ? reader::mreturn("Setup ok!") : reader::mreturn("Error: CFG_1 not used!!!");};

    auto computation =
            logWhichCfg
            then_reader [] (auto) {return doSomethingForCFG_1;};

    auto testReaderMonad = [] () {
        auto helloMonads = reader::runReader(greet, std::string("monads!!"));
        auto helloMeetup = reader::runReader(greet, std::string("C++ meetup!!"));

        std::cout<<"Test reader monad - greet: "<<(std::string)helloMonads<<std::endl;
        std::cout<<"Test reader monad - greet: "<<(std::string)helloMeetup<<std::endl;

        auto errorComp = reader::runReader(computation, CFG_2);
        std::cout<<"Test reader monad - computation: "<<(std::string)errorComp<<std::endl;
        auto successComp = reader::runReader(computation, CFG_1);
        std::cout<<"Test reader monad - computation: "<<(std::string)successComp<<std::endl;
    };
}

namespace continuationMonad{


    auto testContinuation = [] () {

        auto runCont = [] (auto ca, auto cont){
            return ca(cont);
        };
        auto mreturn = [] (auto val){
            return [val] (auto cont) {
                return cont(val);
            };
        };

        auto bind = [runCont] (auto ca, auto f) {
            return [ca, f,runCont] (auto cont) {
                //\k -> c (\a -> runCont (f a) k)
                return ca([f, cont,runCont] (auto x) {return runCont(f(x), cont);});
            };
        };

        //callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k
        auto callCC = [runCont] (auto f){
            return [f,runCont] (auto cont){
                return runCont(f([cont] (auto x) {return [cont, x] (auto) {return cont(x);};}), cont);
            };
        };

        auto computation =
                mreturn(10)
                <<bind>> [=] (auto a) {return mreturn(2)
                <<bind>> [=] (auto b) {return mreturn(a/b);};};

        runCont(computation, [] (auto res) {std::cout<<"Test continuation: Result is "<<res<<std::endl;});

        auto compExposeContinuation =
                mreturn(10)
                <<bind>> [=] (auto a) {return mreturn(2)
                <<bind>> [=] (auto b) {return [=] (auto cont) { /*do extra work*/ return cont(a/b);};};};

        runCont(compExposeContinuation, [] (auto res) {std::cout<<"Test continuation - expose cont: Result is "<<res<<std::endl;});

        auto compIgnoreContinuation = [=] (auto exHandler) {
                return  mreturn(10)
                        <<bind>> [=] (auto a) {return mreturn(0)
                        <<bind>> [=] (auto b) {return [=] (auto cont) {return b == 0 ? exHandler() : cont(a/b);};};};};

        runCont(compIgnoreContinuation([] () {std::cout<<"Test continuation - Exception - division by 0!!"<<std::endl;}),
                [] (auto res) {std::cout<<"Test continuation - compIgnoreContinuation: Result is "<<res<<std::endl;});

    };
}

auto Main = [] () {

    Stack::testStack();
    Accumulate::testAccumulate();
    failingComputations::testFailingComputations();
    errorComputations::testErrorComputations();
    listMonad::testListMonad();
    writerMonad::testLoggingComputations();
    readerMonad::testReaderMonad();
    continuationMonad::testContinuation();
};



int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);
    /******************************/

    Main();

    /******************************/
    return a.exec();
}
