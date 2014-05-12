/*
 * https://atlas.assembla.com/code/vmat/subversion/nodes/109/reves-ann/trunk/src/main/java/net/virtualmat/reves/Delegate.xtend
 */

package org.erlide.annotations

import java.lang.annotation.ElementType
import java.lang.annotation.Target
import org.eclipse.xtend.lib.macro.AbstractClassProcessor
import org.eclipse.xtend.lib.macro.Active
import org.eclipse.xtend.lib.macro.TransformationContext
import org.eclipse.xtend.lib.macro.declaration.MutableClassDeclaration
import org.eclipse.xtend.lib.macro.declaration.TypeDeclaration
import org.eclipse.xtend.lib.macro.declaration.Visibility

@Target(ElementType::TYPE)
@Active(typeof(DelegateParticipant))
annotation Delegate {
    Class<?> to
    String field = "delegate"
}

class DelegateParticipant extends AbstractClassProcessor {

    override doTransform(MutableClassDeclaration annotatedClass, extension TransformationContext context) {
        val annotation = annotatedClass.annotations.filter[
            it.annotationTypeDeclaration.simpleName == Delegate.simpleName].head
        val field = annotation.getStringValue("field")
        val to = annotation.getClassValue("to").type as TypeDeclaration

        process(to, field, annotatedClass, context)
    }

    def process(TypeDeclaration to, String field, MutableClassDeclaration annotatedClass,
        extension TransformationContext context) {
        val methods = to.declaredMethods.filter[it.visibility == Visibility.PUBLIC]

        methods.forEach [ m |
            val exists = annotatedClass.declaredMethods.findFirst [
                it.simpleName == m.simpleName
            ]
            if (exists === null) {
                annotatedClass.addMethod(m.simpleName) [ injected |
                    m.typeParameters.forEach [
                        injected.addTypeParameter(it.simpleName, it.upperBounds.toList)
                    ]
                    m.parameters.forEach [
                        val type = newTypeReference(it.type.type,
                            injected.typeParameters.map[newTypeReference(it)].toList)
                        injected.addParameter(it.simpleName, type)
                    ]
                    injected.returnType = newTypeReference(m.returnType.type,
                        injected.typeParameters.map[newTypeReference(it)].toList)
                    injected.exceptions = m.exceptions
                    injected.body = [
                        '''
                            «IF (!m.returnType.void)»
                                return
                            «ENDIF»
                            «field».«m.simpleName»(
                            «FOR p : m.parameters SEPARATOR ','»«p.simpleName»«ENDFOR»
                            );
                        '''
                    ]
                ]
            }
        ]

    // processes super interfaces and superclasses
    }

}
