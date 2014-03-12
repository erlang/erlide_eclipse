package org.erlide.annotations

import java.lang.annotation.Documented
import java.lang.annotation.ElementType
import java.lang.annotation.Target
import org.eclipse.xtend.lib.macro.AbstractClassProcessor
import org.eclipse.xtend.lib.macro.Active
import org.eclipse.xtend.lib.macro.TransformationContext
import org.eclipse.xtend.lib.macro.declaration.MutableClassDeclaration
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration

@Active(typeof(EqualsAndHashCodeProcessor))
@Documented
@Target(ElementType.TYPE)
annotation EqualsAndHashCode {
}

class EqualsAndHashCodeProcessor extends AbstractClassProcessor {

    override doTransform(MutableClassDeclaration annotatedClass, extension TransformationContext context) {
        super.doTransform(annotatedClass, context)

        annotatedClass.addMethod("hashCode") [
            returnType = Integer.TYPE.newTypeReference()
            body = [
                '''
                    final int prime = 31;
                    int result = super.hashCode();
                    «FOR f : annotatedClass.declaredFields.filter[!static]»
                        «IF Boolean.TYPE.name == f.type.simpleName»
                            result = prime * result + («f.simpleName» ? 1231 : 1237);
                        «ELSEIF Integer.TYPE.name == f.type.simpleName || Character.TYPE.name == f.type.simpleName ||
                        Byte.TYPE.name == f.type.simpleName || Short.TYPE.name == f.type.simpleName»
                            result = prime * result + «f.simpleName»;
                        «ELSEIF Long.TYPE.name == f.type.simpleName»
                            result = prime * result + (int) («f.simpleName» ^ («f.simpleName» >>> 32));
                        «ELSEIF Float.TYPE.name == f.type.simpleName»
                            result = prime * result + Float.floatToIntBits(«f.simpleName»);
                        «ELSEIF Double.TYPE.name == f.type.simpleName»
                            result = prime * result + (int) (Double.doubleToLongBits(«f.simpleName») ^ (Double.doubleToLongBits(«f.simpleName») >>> 32));
                        «ELSE»
                            result = prime * result + ((«f.simpleName»== null) ? 0 : «f.simpleName».hashCode());
                        «ENDIF»
                    «ENDFOR»
                    return 0;
                '''
            ]
        ]
        annotatedClass.addMethod("equals") [
            // annotations.add(toAnnotation(sourceElement, Override.class));
            // parameters.add(toParameter(sourceElement, "obj", references.getTypeForName(Object.class, sourceElement)));
            body = [
                '''
                    if (this == obj)
                        return true;
                    if (obj == null)
                        return false;
                    if (getClass() != obj.getClass())
                        return false;
                '''
            //                if (isDelegateToSuperEquals) {
            //                    p.newLine().append("if (!super.equals(obj))").increaseIndentation();
            //                    p.newLine().append("return false;").decreaseIndentation();
            //                }
            //                p.newLine().append(declaredType.getSimpleName() + " other = (" + declaredType.getSimpleName() + ") obj;");
            //                for (JvmField field : jvmFields) {
            //                    String
            //                    typeName = field.getType().getIdentifier();
            //                    if (Boolean.TYPE.getName().equals(typeName) || Integer.TYPE.getName().equals(typeName) ||
            //                        Long.TYPE.getName().equals(typeName) || Character.TYPE.getName().equals(typeName) ||
            //                        Byte.TYPE.getName().equals(typeName) || Short.TYPE.getName().equals(typeName)) {
            //                        p.newLine().append("if (other." + field.getSimpleName() + " != " + field.getSimpleName() + ")").
            //                            increaseIndentation();
            //                        p.newLine().append("return false;").decreaseIndentation();
            //
            //                    } else if (Double.TYPE.getName().equals(typeName)) {
            //                        p.newLine().append(
            //                            "if (Double.doubleToLongBits(other." + field.getSimpleName() +
            //                                ") != Double.doubleToLongBits(" + field.getSimpleName() + "))").increaseIndentation();
            //                        p.newLine().append("return false;").decreaseIndentation();
            //                    } else if (Float.TYPE.getName().equals(typeName)) {
            //                        p.newLine().append(
            //                            "if (Float.floatToIntBits(other." + field.getSimpleName() + ") != Float.floatToIntBits(" +
            //                                field.getSimpleName() + "))").increaseIndentation();
            //                        p.newLine().append("return false;").decreaseIndentation();
            //                    } else {
            //                        p.newLine().append("if (" + field.getSimpleName() + " == null) {").increaseIndentation();
            //                        p.newLine().append("if (other." + field.getSimpleName() + " != null)").increaseIndentation();
            //                        p.newLine().append("return false;").decreaseIndentation();
            //                        p.decreaseIndentation();
            //                        p.newLine().append(
            //                            "} else if (!" + field.getSimpleName() + ".equals(other." + field.getSimpleName() + "))").
            //                            increaseIndentation();
            //                        p.newLine().append("return false;").decreaseIndentation();
            //                    }
            //                }
            //                p.newLine().append("return true;");
            //                                }
            ]
        ]
    }

}
