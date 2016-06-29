package org.erlide.annotations

import java.lang.annotation.ElementType
import java.lang.annotation.Target
import java.util.ArrayList
import java.util.List
import org.eclipse.xtend.lib.macro.Active
import org.eclipse.xtend.lib.macro.TransformationContext
import org.eclipse.xtend.lib.macro.TransformationParticipant
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy.CompilationContext
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration
import org.eclipse.xtend.lib.macro.declaration.TypeReference
import org.eclipse.xtend.lib.macro.declaration.Visibility

@Target(ElementType.METHOD)
@Active(typeof(MemoizeProcessor))
annotation Memoize {
    long cacheDuration = Long.MAX_VALUE;
    long maxSize = Long.MAX_VALUE;
}

class MemoizeProcessor implements TransformationParticipant<MutableMethodDeclaration> {
    override doTransform(List<? extends MutableMethodDeclaration> methods, extension TransformationContext context) {
        methods.forEach [
            switch (parameters.size) {
                case 0: new ParamterlessMethodMemoizer(it, context, methods.indexOf(it)).generate
                case 1: new SingleParameterMethodMemoizer(it, context, methods.indexOf(it)).generate
                default: new MultipleParameterMethodMemoizer(it, context, methods.indexOf(it)).generate
            }
        ]
    }
}

abstract class MethodMemoizer {

    protected val extension TransformationContext context
    protected val MutableMethodDeclaration method
    val int index

    new(MutableMethodDeclaration method, TransformationContext context, int index) {
        this.method = method
        this.context = context
        this.index = index
    }

    def final generate() {
        method.declaringType => [
            addMethod(initMethodName) [ init |
                init.static = method.static
                init.visibility = Visibility.PRIVATE
                init.returnType = wrappedReturnType
                method.parameters.forEach[init.addParameter(simpleName, type)]
                init.exceptions = method.exceptions
                init.body = method.body
            ]
            addField(cacheFieldName) [
                static = method.static
                type = cacheFieldType
                initializer = [cacheFieldInit]
            ]
        ]
        method => [
            body = [cacheCall]
            returnType = wrappedReturnType
        ]
    }

    def protected final wrappedReturnType() {
        method.returnType.wrapperIfPrimitive
    }

    def protected final initMethodName() {
        method.simpleName + "_init"
    }

    def protected final String cacheFieldName() '''cache«index»_«method.simpleName»'''

    def protected CharSequence cacheCall(CompilationContext context)

    def protected TypeReference cacheFieldType()

    def protected CharSequence cacheFieldInit(CompilationContext context)
}

class ParamterlessMethodMemoizer extends MethodMemoizer {

    new(MutableMethodDeclaration method, TransformationContext context, int index) {
        super(method, context, index)
    }

    override protected cacheCall(extension CompilationContext context) '''
        if («cacheFieldName» == null) {
          synchronized(«lock») {
            if («cacheFieldName» == null) {
              «cacheFieldName» = «initMethodName»();
            }
          }
        }
        return «cacheFieldName»;
    '''

    override protected cacheFieldType() {
        wrappedReturnType
    }

    override protected cacheFieldInit(CompilationContext context) '''null'''

    def lock() {
        if (method.static) '''«method.declaringType.simpleName».class''' else "this"
    }
}

abstract class ParametrizedMethodMemoizer extends MethodMemoizer {
    new(MutableMethodDeclaration method, TransformationContext context, int index) {
        super(method, context, index)
    }

    override protected final cacheFieldInit(extension CompilationContext context) {
        val memoizeAnnotation = method.findAnnotation(typeof(Memoize).newTypeReference.type)
        val cacheDuration = memoizeAnnotation.getValue("cacheDuration")
        val maxSize = memoizeAnnotation.getValue("maxSize")

        '''
            com.google.common.cache.CacheBuilder.newBuilder()
            .expireAfterAccess(«cacheDuration», «toJavaCode(newTypeReference("java.util.concurrent.TimeUnit"))».MILLISECONDS)
            .maximumSize(«maxSize»)
            .build(new com.google.common.cache.CacheLoader«cacheKeyType.toJavaCode», «wrappedReturnType.toJavaCode»>() {
              @Override
              public «wrappedReturnType.toJavaCode» load(«cacheKeyType.toJavaCode» key) throws Exception {
                return «initMethodName»(«cacheKeyToParameters(context)»);
              }
            })
        '''
    }

    override protected final cacheFieldType() {
        newTypeReference(
            "com.google.common.cache.LoadingCache",
            cacheKeyType,
            wrappedReturnType
        )
    }

    override protected final cacheCall(extension CompilationContext context) '''
        try {
          return «cacheFieldName».get(«parametersToCacheKey(context)»);
        } catch (Throwable e) {
          throw «typeof(Exceptions).newTypeReference.toJavaCode».sneakyThrow(e.getCause());
        }
    '''

    def protected TypeReference cacheKeyType()

    def protected CharSequence parametersToCacheKey(CompilationContext context)

    def protected CharSequence cacheKeyToParameters(CompilationContext context)
}

class SingleParameterMethodMemoizer extends ParametrizedMethodMemoizer {
    new(MutableMethodDeclaration method, TransformationContext context, int index) {
        super(method, context, index)
    }

    override protected cacheKeyToParameters(CompilationContext context) '''key'''

    override protected parametersToCacheKey(CompilationContext context) {
        parameter.simpleName
    }

    override protected cacheKeyType() {
        parameter.type.wrapperIfPrimitive
    }

    def private parameter() {
        method.parameters.head
    }
}

class MultipleParameterMethodMemoizer extends ParametrizedMethodMemoizer {
    new(MutableMethodDeclaration method, TransformationContext context, int index) {
        super(method, context, index)
    }

    override protected cacheKeyToParameters(extension CompilationContext context) {
        (method.parameters).join("", ",", "")[
            '''
        («type.toJavaCode») key.getParameters()[«method.parameters.toList.indexOf(it)»]
            ''']
    }

    override protected parametersToCacheKey(extension CompilationContext context) '''
        new «cacheKeyType.toJavaCode»(«method.parameters.join("", ",", "")[simpleName]»)
    '''

    override protected cacheKeyType() {
        typeof(CacheKey).newTypeReference
    }
}

class CacheKey extends ArrayList<Object> implements List<Object> {
}
