

auto compose = [] (auto f){
    return [f] (auto g){
        return [f, g] (auto x){
            return f(g(x));
        };
    };
};

auto zero = [] (auto f) {
    return [] (auto x){
        return x;
    };
};

auto one = [] (auto f) {
    return [f] (auto x){
        return f(x);
    };
};

auto two = [] (auto f) {
    return [f] (auto x){
        return f(f(x));
    };
};

auto three = [] (auto f) {
    return [f] (auto x){
        return f(f(f(x)));
    };
};

auto toNumRepr = [] (auto fn) {
    return fn([] (auto x) {return x+1;})(0);
};

/* increment */
auto increment = [] (auto fn) {
    return [fn] (auto f) {
        return [fn, f] (auto x){
            return f(fn(f)(x));
        };
    };
};

/* decrement */
auto decrement = [] (auto fn) {
    return [fn] (auto f) {
        return [fn, f] (auto x){
            return fn([f] (auto g){
                return [f, g] (auto h) {
                    return h(g(f));
                };
            })([x] (auto u) {return x;})([] (auto x) {return x;});
        };
    };
};

/* add */
auto add = [] (auto fn1) {
    return [fn1] (auto fn2){
        return fn2(increment)(fn1);
    };
};

/* sub */
auto sub = [] (auto fn1) {
    return [fn1] (auto fn2) {
        return fn2(decrement)(fn1);
    };
};

/* mul */
auto mul = [] (auto fn1){
    return [fn1] (auto fn2) {
        return [fn1, fn2] (auto f){
            return fn1(fn2(f));
        };
    };
};

/* booleans */
auto truef = [] (auto x) {
    return [x] (auto y){
        return x;
    };
};

auto falsef = [] (auto x) {
    return [] (auto y){
        return y;
    };
};

auto ifte = [] (auto boolean){
    return [boolean] (auto t){
        return [boolean, t] (auto f) {
            return boolean(t)(f);
        };
    };
};

auto isZero = [] (auto fn) {
    return fn([] (auto u) {return falsef;})(truef);
};

/*
auto fact = [] (auto fn){
    return ifte(isZero(fn))
            (one)
            (mul(fn)(fact(decrement(fn))));
};
*/

/* Pairs */
auto pair = [] (auto x) {
    return [x] (auto y) {
        return [x,y] (auto z){
            return z(x)(y);
        };
    };
};

auto first = [] (auto p) {
    return p([] (auto x){
        return [x] (auto y){
            return x;
        };
    });
};

auto second = [] (auto p) {
    return p([] (auto x){
        return [] (auto y){
            return y;
        };
    });
};

auto fact = [] (auto fn) {
    return first(decrement(fn)([] (auto p) {
        return pair(mul(first(p)(increment(second(p)))))(increment(second(p)));
    })(pair(mul(one))(one)))(one);
};

