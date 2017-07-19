package org.erlide.annotations

import java.lang.annotation.Documented
import java.lang.annotation.ElementType
import java.lang.annotation.Target
import org.eclipse.xtend.lib.macro.AbstractClassProcessor
import org.eclipse.xtend.lib.macro.Active
import org.eclipse.xtend.lib.macro.RegisterGlobalsContext
import org.eclipse.xtend.lib.macro.TransformationContext
import org.eclipse.xtend.lib.macro.declaration.ClassDeclaration
import org.eclipse.xtend.lib.macro.declaration.MutableClassDeclaration

@Active(typeof(ImmutableProcessor))
@Documented
@Target(ElementType.TYPE)
annotation Immutable {
}

class ImmutableProcessor extends AbstractClassProcessor {

  override doRegisterGlobals(ClassDeclaration cls, RegisterGlobalsContext context) {
    context.registerClass(cls.builderClassName)
  }

  override doTransform(MutableClassDeclaration cls, extension TransformationContext context) {
    if(cls.extendedClass != object) cls.addError("Inheritance does not play well with immutability")
    cls.final = true

    val builder = cls.builderClassName.findClass => [
      final = true
      addMethod("build") [
        returnType = cls.newTypeReference
        body = [
          '''
            return new «cls.simpleName»(«cls.dataFields.join(",")[simpleName]»);
          ''']
      ]
      cls.dataFields.forEach [ field |
        addMethod(field.simpleName) [
          addParameter(field.simpleName, field.type)
          returnType = cls.builderClassName.newTypeReference
          body = [
            '''
              this.«field.simpleName» = «field.simpleName»;
              return this;
            ''']
        ]
        addField(field.simpleName) [
          type = field.type
        ]
      ]
    ]

    cls.addMethod("build") [
      static = true
      returnType = cls.newTypeReference
      addParameter("init", typeof(Procedures.Procedure1).newTypeReference(builder.newTypeReference))
      body = [
        '''
          «cls.builderClassName» builder = builder();
          init.apply(builder);
          return builder.build();
        ''']
    ]
    cls.addMethod("builder") [
      returnType = cls.builderClassName.newTypeReference
      static = true
      body = [
        '''
          return new «cls.builderClassName»();
        ''']
    ]

    cls.addConstructor [
      cls.dataFields.forEach [ field |
        addParameter(field.simpleName, field.type)
      ]
      body = [
        '''
          «FOR p : cls.dataFields»
            this.«p.simpleName» = «p.simpleName»;
          «ENDFOR»
        ''']
    ]
    cls.dataFields.forEach [ field |
      val fieldType = field.type
      val prefix = if(fieldType == primitiveBoolean || fieldType.type.simpleName == "Boolean") "is" else "get"
      cls.addMethod(prefix + field.simpleName.toFirstUpper) [
        returnType = field.type
        body = [
          '''
            return «field.simpleName»;
          ''']
      ]
    ]
    cls.addMethod("equals") [
      returnType = primitiveBoolean
      addParameter("o", object)
      val result = cls.dataFields.join("\n&& ")['''«objects».equal(«simpleName», other.«simpleName»)''']
      body = [
        '''
          if (o instanceof «cls.simpleName») {
            «cls.simpleName» other = («cls.simpleName») o;
            return «result»;
          }
          return false;
        ''']
    ]
    cls.addMethod("hashCode") [
      returnType = primitiveInt
      body = ['''return «objects».hashCode(«cls.dataFields.join(",")[simpleName]»);''']
    ]
    cls.addMethod("toString") [
      returnType = string
      body = ['''return new org.eclipse.xtext.xbase.lib.util.ToStringHelper().toString(this);''']
    ]
  }

  def dataFields(MutableClassDeclaration cls) {
    cls.declaredFields.filter[static == false]
  }

  def builderClassName(ClassDeclaration cls) {
    cls.qualifiedName + "Builder"
  }

  def objects() {
    "com.google.common.base.Objects"
  }
}
