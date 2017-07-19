package nl.kii.util

class ThrowableExtensions {

    def static format(Throwable it) {
        it.format(null)
    }

    def static String format(Throwable it, String message) '''
        «IF message !== null»Thrown: «message»«ENDIF»
        Thrown «class.simpleName» - «message»:
            «it.message»
            «FOR trace : stackTrace.map[toString]»
            - «trace»
            «ENDFOR»
        «IF cause !== null»
            Caused by «cause.class.simpleName»:
                «cause.message»
                «FOR trace : cause.stackTrace.map[toString]»
                - «trace»
                «ENDFOR»
        «ENDIF»
    '''

}
