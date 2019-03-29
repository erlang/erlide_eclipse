package org.erlide.util

import java.lang.annotation.ElementType
import java.lang.annotation.Retention
import java.lang.annotation.Target
import java.lang.ref.WeakReference
import java.util.WeakHashMap
import org.eclipse.xtend.lib.macro.AbstractClassProcessor
import org.eclipse.xtend.lib.macro.Active
import org.eclipse.xtend.lib.macro.TransformationContext
import org.eclipse.xtend.lib.macro.declaration.MutableClassDeclaration
import org.eclipse.xtend.lib.macro.declaration.Visibility
import org.eclipse.xtend.lib.macro.declaration.ClassDeclaration
import org.eclipse.xtend.lib.macro.RegisterGlobalsContext

@Target(ElementType.TYPE)
@Retention(SOURCE)
@Active(WrappedProcessor)
annotation Wrapped {
    public boolean cached = false
    public boolean synch = false
}

interface Wrapper<T> {
}

class WrappedProcessor extends AbstractClassProcessor {

    override doRegisterGlobals(ClassDeclaration annotatedClass, extension RegisterGlobalsContext context) {
        //context.registerClass(annotatedClass.qualifiedName + "_")
    }

    override doTransform(MutableClassDeclaration annotatedClass, extension TransformationContext context) {

        val is_cached = annotatedClass.annotations.head.getBooleanValue("cached")
        val is_synchronized = annotatedClass.annotations.head.getBooleanValue("synch")

        val intfs = annotatedClass.implementedInterfaces
        if (intfs.size != 1) {
            addError(annotatedClass, "Only one interface must be specified for a wrapped class")
        }
        if (intfs.head.type.simpleName != "Wrapper") {
            addError(annotatedClass, "Wrapped classes must implement Wrapper<T> " + intfs.head.type.simpleName)
        }
        val typeArgs = intfs.head.actualTypeArguments
        val argType0 = typeArgs.head
        val argType = argType0.primitiveIfWrapper

        annotatedClass.final = true
        annotatedClass.addField("value") [
            final = true
            type = argType
        ]
        if (is_cached) {
            annotatedClass.addField("cache") [
                static = true
                type = WeakPool.newTypeReference(argType, annotatedClass.newTypeReference())
                initializer = '''
					new WeakPool<>()
				'''
            ]
        }

        annotatedClass.addConstructor [
            it.visibility = Visibility.PRIVATE
            it.addParameter("value", argType)
            it.body = '''
				this.value = value;
			'''
        ]
        annotatedClass.addMethod("of") [
            static = true
            synchronized = is_synchronized
            addParameter("value", argType)
            returnType = annotatedClass.newSelfTypeReference
            body = '''
				«IF !argType.primitive»
					if (value == null)
					    throw new NullPointerException("Wrapped value must not be null");
				«ENDIF»
				«IF is_cached»
					«annotatedClass.newSelfTypeReference» v = cache.get(value);
					if(v == null) {
					    v = new «annotatedClass.newSelfTypeReference»(value);
					    cache.put(value, v);
					}
				«ELSE»
					«annotatedClass.newSelfTypeReference» v = new «annotatedClass.newSelfTypeReference»(value);
				«ENDIF»
				return v;
			'''
        ]
        annotatedClass.addMethod("value") [
            returnType = argType
            body = '''
				return value;
			'''
        ]
        annotatedClass.addMethod("toString") [
            addAnnotation(Override.newAnnotationReference())
            returnType = string
            body = '''
				return "«annotatedClass.simpleName»(" + value + ")";
			'''
        ]
        annotatedClass.addMethod("equals") [
            addAnnotation(Override.newAnnotationReference())
            addParameter("other", Object.newTypeReference())
            returnType = boolean.newTypeReference()
            body = '''
				«IF argType.primitive»
					return java.util.Objects.equals(value, ((«annotatedClass.newSelfTypeReference»)other).value);
				«ELSE»
					return java.util.Objects.equals(value, ((«annotatedClass.newSelfTypeReference»)other).value);
				«ENDIF»
			'''
        ]
        annotatedClass.addMethod("hashCode") [
            addAnnotation(Override.newAnnotationReference())
            returnType = int.newTypeReference()
            body = '''
				«IF argType.primitive»
					return «argType0.simpleName».hashCode(value);
				«ELSE»
					return value.hashCode();
				«ENDIF»
			'''
        ]
    }
}

// This must be available at runtime!

class WeakPool<K, T> {
    val pool = new WeakHashMap<K, WeakReference<T>>()

    def T get(K key) {
        val ref = pool.get(key)
        if (ref !== null) {
            ref.get()
        } else {
            null
        }
    }

    def put(K key, T object) {
        pool.put(key, new WeakReference<T>(object))
    }
}
