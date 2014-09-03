package org.erlide.annotations

import java.lang.annotation.ElementType
import java.lang.annotation.Target
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtend.lib.macro.AbstractClassProcessor
import org.eclipse.xtend.lib.macro.Active
import org.eclipse.xtend.lib.macro.TransformationContext
import org.eclipse.xtend.lib.macro.declaration.MutableClassDeclaration

@Target(ElementType.TYPE)
@Active(typeof(WitherParticipant))
annotation Wither {
}

class WitherParticipant extends AbstractClassProcessor {

  override doTransform(MutableClassDeclaration annotatedClass, extension TransformationContext context) {

    val dataClass = annotatedClass.annotations.exists[
      typeof(Data).canonicalName == it.annotationTypeDeclaration.qualifiedName]
    if (!dataClass) {
      addError(annotatedClass, "Class needs to be annotated with @Data to be able to use @Wither");
      return;
    }

    val fields = annotatedClass.declaredFields.filter[!static]

    fields.forEach [ field |
      val properName = field.simpleName.substring(1) // remove underscore prefix
      val capitalizedName = properName.toFirstUpper
      val methodName = "with" + capitalizedName
      if (!annotatedClass.declaredMethods.exists[simpleName == methodName]) {
        annotatedClass.addMethod(methodName) [
          addParameter(properName, field.type)
          setReturnType(newTypeReference(annotatedClass))
          body = [
            '''
              return new «annotatedClass.simpleName»(
              «FOR f : fields SEPARATOR ','»
                «IF f == field»
                  «properName»
                «ELSE»
                  «f.simpleName»
                «ENDIF»
              «ENDFOR»
              );
            '''
          ]
        ]
      }
    ]
  }

}
