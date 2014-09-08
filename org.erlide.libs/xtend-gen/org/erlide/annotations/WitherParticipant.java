package org.erlide.annotations;

import com.google.common.base.Objects;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtend.lib.macro.AbstractClassProcessor;
import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.AnnotationReference;
import org.eclipse.xtend.lib.macro.declaration.AnnotationTypeDeclaration;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.MutableClassDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableFieldDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.StringExtensions;

@SuppressWarnings("all")
public class WitherParticipant extends AbstractClassProcessor {
  public void doTransform(final MutableClassDeclaration annotatedClass, @Extension final TransformationContext context) {
    Iterable<? extends AnnotationReference> _annotations = annotatedClass.getAnnotations();
    final Function1<AnnotationReference, Boolean> _function = new Function1<AnnotationReference, Boolean>() {
      public Boolean apply(final AnnotationReference it) {
        String _canonicalName = Data.class.getCanonicalName();
        AnnotationTypeDeclaration _annotationTypeDeclaration = it.getAnnotationTypeDeclaration();
        String _qualifiedName = _annotationTypeDeclaration.getQualifiedName();
        return Boolean.valueOf(Objects.equal(_canonicalName, _qualifiedName));
      }
    };
    final boolean dataClass = IterableExtensions.exists(_annotations, _function);
    if ((!dataClass)) {
      context.addError(annotatedClass, "Class needs to be annotated with @Data to be able to use @Wither");
      return;
    }
    Iterable<? extends MutableFieldDeclaration> _declaredFields = annotatedClass.getDeclaredFields();
    final Function1<MutableFieldDeclaration, Boolean> _function_1 = new Function1<MutableFieldDeclaration, Boolean>() {
      public Boolean apply(final MutableFieldDeclaration it) {
        boolean _isStatic = it.isStatic();
        return Boolean.valueOf((!_isStatic));
      }
    };
    final Iterable<? extends MutableFieldDeclaration> fields = IterableExtensions.filter(_declaredFields, _function_1);
    final Procedure1<MutableFieldDeclaration> _function_2 = new Procedure1<MutableFieldDeclaration>() {
      public void apply(final MutableFieldDeclaration field) {
        String _simpleName = field.getSimpleName();
        final String properName = _simpleName.substring(1);
        final String capitalizedName = StringExtensions.toFirstUpper(properName);
        final String methodName = ("with" + capitalizedName);
        Iterable<? extends MutableMethodDeclaration> _declaredMethods = annotatedClass.getDeclaredMethods();
        final Function1<MutableMethodDeclaration, Boolean> _function = new Function1<MutableMethodDeclaration, Boolean>() {
          public Boolean apply(final MutableMethodDeclaration it) {
            String _simpleName = it.getSimpleName();
            return Boolean.valueOf(Objects.equal(_simpleName, methodName));
          }
        };
        boolean _exists = IterableExtensions.exists(_declaredMethods, _function);
        boolean _not = (!_exists);
        if (_not) {
          final Procedure1<MutableMethodDeclaration> _function_1 = new Procedure1<MutableMethodDeclaration>() {
            public void apply(final MutableMethodDeclaration it) {
              TypeReference _type = field.getType();
              it.addParameter(properName, _type);
              TypeReference _newTypeReference = context.newTypeReference(annotatedClass);
              it.setReturnType(_newTypeReference);
              final CompilationStrategy _function = new CompilationStrategy() {
                public CharSequence compile(final CompilationStrategy.CompilationContext it) {
                  StringConcatenation _builder = new StringConcatenation();
                  _builder.append("return new ");
                  String _simpleName = annotatedClass.getSimpleName();
                  _builder.append(_simpleName, "");
                  _builder.append("(");
                  _builder.newLineIfNotEmpty();
                  {
                    boolean _hasElements = false;
                    for(final MutableFieldDeclaration f : fields) {
                      if (!_hasElements) {
                        _hasElements = true;
                      } else {
                        _builder.appendImmediate(",", "");
                      }
                      {
                        boolean _equals = Objects.equal(f, field);
                        if (_equals) {
                          _builder.append(properName, "");
                          _builder.newLineIfNotEmpty();
                        } else {
                          String _simpleName_1 = f.getSimpleName();
                          _builder.append(_simpleName_1, "");
                          _builder.newLineIfNotEmpty();
                        }
                      }
                    }
                  }
                  _builder.append(");");
                  _builder.newLine();
                  return _builder;
                }
              };
              it.setBody(_function);
            }
          };
          annotatedClass.addMethod(methodName, _function_1);
        }
      }
    };
    IterableExtensions.forEach(fields, _function_2);
  }
}
