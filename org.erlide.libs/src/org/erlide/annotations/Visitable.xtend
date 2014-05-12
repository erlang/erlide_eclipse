package org.erlide.annotations

import java.lang.annotation.Documented
import java.lang.annotation.ElementType
import java.lang.annotation.Target
import java.util.List
import java.util.Set
import org.eclipse.xtend.lib.macro.Active
import org.eclipse.xtend.lib.macro.RegisterGlobalsContext
import org.eclipse.xtend.lib.macro.RegisterGlobalsParticipant
import org.eclipse.xtend.lib.macro.TransformationContext
import org.eclipse.xtend.lib.macro.TransformationParticipant
import org.eclipse.xtend.lib.macro.declaration.ClassDeclaration
import org.eclipse.xtend.lib.macro.declaration.CompilationUnit
import org.eclipse.xtend.lib.macro.declaration.MethodDeclaration
import org.eclipse.xtend.lib.macro.declaration.MutableClassDeclaration
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration
import org.eclipse.xtend.lib.macro.declaration.MutableTypeDeclaration
import org.eclipse.xtend.lib.macro.declaration.TypeDeclaration
import org.eclipse.xtend.lib.macro.declaration.TypeReference
import org.eclipse.xtend.lib.macro.declaration.Visibility

@Active(typeof(VisitableProcessor))
@Documented
@Target(ElementType.TYPE)
annotation Visitable {
}

class VisitableProcessor implements RegisterGlobalsParticipant<TypeDeclaration>, TransformationParticipant<MutableTypeDeclaration> {

    override doRegisterGlobals(List<? extends TypeDeclaration> types, RegisterGlobalsContext context) {
        types.forEach [
            acceptMethods.forEach [
                context.registerClass(visitorName)
            ]
        ]
    }

    private def getAcceptMethods(TypeDeclaration it) {
        declaredMembers.filter(typeof(MethodDeclaration)).filter[
            simpleName.startsWith('accept') && parameters.size >= 1]
    }

    private def getVisitorName(MethodDeclaration it) {
        parameters.head.type.type.qualifiedName
    }

    override doTransform(List<? extends MutableTypeDeclaration> types, extension TransformationContext context) {
        types.forEach [ root |
            val allInheritors = root.newTypeReference.getInheritorsDeclaredIn(root.compilationUnit, context)
            root.acceptMethods.forEach [ method |
                val visitor = findClass(method.visitorName)
                visitor.addVisitMethods(allInheritors, method, context)
                allInheritors.map[primaryGeneratedJavaElement as MutableClassDeclaration].forEach [
                    val existingMethod = findDeclaredMethod(method.simpleName, method.parameters.map[type])
                    if (existingMethod === null) {
                        addMethod(method.simpleName) [ newMethod |
                            newMethod.returnType = method.returnType
                            method.parameters.forEach [
                                newMethod.addParameter(simpleName, type)
                            ]
                            newMethod.body = [
                                newMethod.acceptMethodBody
                            ]
                        ]
                    } else {
                        existingMethod => [
                            if (root == declaringType) {
                                abstract = false
                                body = [ cu |
                                    acceptMethodBody
                                ]
                            }
                        ]
                    }
                ]
            ]
        ]
    }

    private def getAcceptMethodBody(MutableMethodDeclaration it) {
        '''«IF !returnType.void»return «ENDIF»«parameters.head.simpleName».visit«declaringType.simpleName»(this«parameters.toList.
            subList(1, parameters.size).join(', ', ', ', '')[simpleName]»);'''
    }

    private def addVisitMethods(
        MutableClassDeclaration clazz,
        Set<? extends ClassDeclaration> inheritors,
        MethodDeclaration original,
        extension TransformationContext context
    ) {
        clazz.visibility = Visibility.PUBLIC
        inheritors.forEach [ inheritor |
            clazz.addMethod('visit' + inheritor.simpleName) [ method |
                method.returnType = original.returnType
                method.addParameter('visitable', inheritor.newTypeReference)
                original.parameters.drop(1).forEach [
                    method.addParameter(simpleName, type)
                ]
                if (clazz == inheritor || inheritor.extendedClass === null) {
                    if (original.body !== null)
                        method.body = original.body
                    else
                        method.setBody [
                            '''throw new IllegalStateException();'''
                        ]
                } else {
                    method.setBody [
                        '''«IF !original.returnType.void»return «ENDIF»visit«(inheritor.extendedClass.type as TypeDeclaration).
                            simpleName»(«method.parameters.map[simpleName].join(', ')»);'''
                    ]
                }
            ]
        ]
    }

    private def getInheritorsDeclaredIn(TypeReference typeReference, CompilationUnit it,
        extension TransformationContext context) {
        sourceTypeDeclarations.filter [
            typeReference.isAssignableFrom(newTypeReference)
        ].filter(typeof(ClassDeclaration)).toSet
    }

}
