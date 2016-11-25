#include <QCoreApplication>

#include <iostream>

#include "church.cpp"

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    auto random_computation = [] (auto fn) {
        return ifte(isZero(fn))
                (add(increment(fn))(three))
                (mul(decrement(fn))(three));
    };

    std::cout<<"Random computation result: "<<toNumRepr(random_computation(two))<<std::endl;

    auto ten = [] (auto f){
        return [f] (auto x){
            return f(f(f(f(f(f(f(f(f(f(x))))))))));
        };
    };

    std::cout<<"Factorial result for 10: "<<toNumRepr(fact(ten))<<std::endl;

    return a.exec();
}

#if 0
template<typename ret, typename first, typename ... rest>
function<function<ret(rest...)>(first)> curry(function<ret(first, rest...)> f){
    return [f] (first firstParam) -> function<ret(rest...)>{
        return [f, firstParam] (rest... restParam) -> ret{
            return f(firstParam, restParam...);
        };
    };
}
#endif
