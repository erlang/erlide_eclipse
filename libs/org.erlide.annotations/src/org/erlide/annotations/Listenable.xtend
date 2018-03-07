package org.erlide.annotations

import java.lang.annotation.ElementType
import java.lang.annotation.Target
import java.util.ArrayList
import org.eclipse.xtend.lib.macro.AbstractFieldProcessor
import org.eclipse.xtend.lib.macro.Active
import org.eclipse.xtend.lib.macro.TransformationContext
import org.eclipse.xtend.lib.macro.declaration.MutableFieldDeclaration
import org.eclipse.xtend.lib.macro.declaration.Visibility
import org.eclipse.xtext.xbase.lib.Functions.Function0
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1

/**
 * Transforms the field to a listenerList. Also two methods will be added: 
 * 1. addBallEventListener((fieldType)=>void listener)
 * 2. notifyAllBallEventListener(fieldType event) 
 */
@Target(ElementType.FIELD)
@Active(ListenerProcessor)
annotation Listenable {
}

class ListenerProcessor extends AbstractFieldProcessor {

    override doTransform(MutableFieldDeclaration field, extension TransformationContext context) {
        if (field.type.primitive) {
            field.addError("Primitives can't be used as event.")
        }
        if (field.initializer !== null) {
            field.addError("Listener event dont't need an initializer")
        }
        
        val eventType = field.type
        val eventListType = Procedure1.newTypeReference(eventType).list
        
        field.setType(eventListType)
        field.simpleName = field.simpleName + "Listener"
        
        val lamdaType = Procedure1.newTypeReference(eventType)
        val initFieldType = Function0.newTypeReference(lamdaType.list)
        val initListType = ArrayList.newTypeReference(lamdaType)
        
        field.setInitializer(
            [
                '''new «toJavaCode(initFieldType)»() {
                public «toJavaCode(eventListType)» apply() {
                «toJavaCode(eventListType)» _eventList = new «toJavaCode(initListType)»();
                    return _eventList;
                }
            }.apply();''']
        )

        field.declaringType.addMethod("add" + field.simpleName.toFirstUpper) [
            visibility = Visibility.PUBLIC
            addParameter("listener", lamdaType)
            body = [
                '''this.«field.simpleName».add(listener);'''
            ]
        ]

        field.declaringType.addMethod("notifyAll" + field.simpleName.toFirstUpper) [
            visibility = Visibility.PUBLIC
            addParameter("event", eventType)
            body = [
                '''for («toJavaCode(lamdaType)» listener : «field.simpleName») {
                    listener.apply(event);
                    }'''
            ]
        ]
    }
}
