package nl.kii.util

import org.eclipse.xtext.xbase.lib.Functions.Function0
import org.eclipse.xtext.xbase.lib.Functions.Function1
import org.eclipse.xtext.xbase.lib.Functions.Function2
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1

class FunctionExtensions {

    // OPERATOR OVERLOADING ///////////////////////////////////////////////////
    /** Negate the result of a function */
    def static Function0<Boolean> !(Function0<Boolean> fn) {
        [|!fn.apply]
    }

    /** Negate the result of a function */
    def static <P1> Function1<? super P1, Boolean> !(Function1<? super P1, Boolean> fn) {
        [!fn.apply(it)]
    }

    /** Negate the result of a function */
    def static <P1, P2> Function2<? super P1, ? super P2, Boolean> !(Function2<? super P1, ? super P2, Boolean> fn) {
        [a, b|!fn.apply(a, b)]
    }

    /** Let the << operator also work on Procedure1.apply */
    def static <Param> <<(Procedure1<Param> fn, Param p) {
        fn.apply(p)
    }

    /** Let the << operator also work on Function1.apply */
    def static <Param, T> T <<(Function1<Param, T> fn, Param p) {
        fn.apply(p)
    }

    /** Let the >> operator also work on Procedure1.apply */
    def static <Param> >>(Param p, (Param)=>void fn) {
        fn.apply(p)
    }

    /** Let the >> operator also work on Function1.apply */
    def static <Param, T> T >>(Param p, (Param)=>T fn) {
        fn.apply(p)
    }

    /** Let the _ operator also work on Procedure1.apply */
    def static <Param> _(Procedure1<Param> fn, Param p) {
        fn.apply(p)
    }

    /** Let the _ operator also work on Function1.apply */
    def static <Param, T> T _(Function1<Param, T> fn, Param p) {
        fn.apply(p)
    }

    /** Let the _ operator also work on Procedure1.apply */
    def static <Param> _(Param p, (Param)=>void fn) {
        fn.apply(p)
    }

    /** Let the _ operator also work on Function1.apply */
    def static <Param, T> T _(Param p, (Param)=>T fn) {
        fn.apply(p)
    }

}
