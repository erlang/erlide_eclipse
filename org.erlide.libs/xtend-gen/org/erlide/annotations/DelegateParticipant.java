/**
 * https://atlas.assembla.com/code/vmat/subversion/nodes/109/reves-ann/trunk/src/main/java/net/virtualmat/reves/Delegate.xtend
 */
package org.erlide.annotations;

import com.google.common.base.Objects;
import java.util.List;
import org.eclipse.xtend.lib.macro.AbstractClassProcessor;
import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.AnnotationReference;
import org.eclipse.xtend.lib.macro.declaration.AnnotationTypeDeclaration;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.MethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableClassDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableTypeParameterDeclaration;
import org.eclipse.xtend.lib.macro.declaration.ParameterDeclaration;
import org.eclipse.xtend.lib.macro.declaration.Type;
import org.eclipse.xtend.lib.macro.declaration.TypeDeclaration;
import org.eclipse.xtend.lib.macro.declaration.TypeParameterDeclaration;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend.lib.macro.declaration.Visibility;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.annotations.Delegate;

@SuppressWarnings("all")
public class DelegateParticipant extends AbstractClassProcessor {
  public void doTransform(final MutableClassDeclaration annotatedClass, @Extension final TransformationContext context) {
    Iterable<? extends AnnotationReference> _annotations = annotatedClass.getAnnotations();
    final Function1<AnnotationReference, Boolean> _function = new Function1<AnnotationReference, Boolean>() {
      public Boolean apply(final AnnotationReference it) {
        AnnotationTypeDeclaration _annotationTypeDeclaration = it.getAnnotationTypeDeclaration();
        String _simpleName = _annotationTypeDeclaration.getSimpleName();
        String _simpleName_1 = Delegate.class.getSimpleName();
        return Boolean.valueOf(Objects.equal(_simpleName, _simpleName_1));
      }
    };
    Iterable<? extends AnnotationReference> _filter = IterableExtensions.filter(_annotations, _function);
    final AnnotationReference annotation = IterableExtensions.head(_filter);
    final String field = annotation.getStringValue("field");
    TypeReference _classValue = annotation.getClassValue("to");
    Type _type = _classValue.getType();
    final TypeDeclaration to = ((TypeDeclaration) _type);
    this.process(to, field, annotatedClass, context);
  }
  
  public void process(final TypeDeclaration to, final String field, final MutableClassDeclaration annotatedClass, @Extension final TransformationContext context) {
    Iterable<? extends MethodDeclaration> _declaredMethods = to.getDeclaredMethods();
    final Function1<MethodDeclaration, Boolean> _function = new Function1<MethodDeclaration, Boolean>() {
      public Boolean apply(final MethodDeclaration it) {
        Visibility _visibility = it.getVisibility();
        return Boolean.valueOf(Objects.equal(_visibility, Visibility.PUBLIC));
      }
    };
    final Iterable<? extends MethodDeclaration> methods = IterableExtensions.filter(_declaredMethods, _function);
    final Procedure1<MethodDeclaration> _function_1 = new Procedure1<MethodDeclaration>() {
      public void apply(final MethodDeclaration m) {
        Iterable<? extends MutableMethodDeclaration> _declaredMethods = annotatedClass.getDeclaredMethods();
        final Function1<MutableMethodDeclaration, Boolean> _function = new Function1<MutableMethodDeclaration, Boolean>() {
          public Boolean apply(final MutableMethodDeclaration it) {
            String _simpleName = it.getSimpleName();
            String _simpleName_1 = m.getSimpleName();
            return Boolean.valueOf(Objects.equal(_simpleName, _simpleName_1));
          }
        };
        final MutableMethodDeclaration exists = IterableExtensions.findFirst(_declaredMethods, _function);
        boolean _tripleEquals = (exists == null);
        if (_tripleEquals) {
          String _simpleName = m.getSimpleName();
          final Procedure1<MutableMethodDeclaration> _function_1 = new Procedure1<MutableMethodDeclaration>() {
            public void apply(final MutableMethodDeclaration injected) {
              Iterable<? extends TypeParameterDeclaration> _typeParameters = m.getTypeParameters();
              final Procedure1<TypeParameterDeclaration> _function = new Procedure1<TypeParameterDeclaration>() {
                public void apply(final TypeParameterDeclaration it) {
                  String _simpleName = it.getSimpleName();
                  Iterable<? extends TypeReference> _upperBounds = it.getUpperBounds();
                  List<? extends TypeReference> _list = IterableExtensions.toList(_upperBounds);
                  injected.addTypeParameter(_simpleName, ((TypeReference[])Conversions.unwrapArray(_list, TypeReference.class)));
                }
              };
              IterableExtensions.forEach(_typeParameters, _function);
              Iterable<? extends ParameterDeclaration> _parameters = m.getParameters();
              final Procedure1<ParameterDeclaration> _function_1 = new Procedure1<ParameterDeclaration>() {
                public void apply(final ParameterDeclaration it) {
                  TypeReference _type = it.getType();
                  Type _type_1 = _type.getType();
                  Iterable<? extends MutableTypeParameterDeclaration> _typeParameters = injected.getTypeParameters();
                  final Function1<MutableTypeParameterDeclaration, TypeReference> _function = new Function1<MutableTypeParameterDeclaration, TypeReference>() {
                    public TypeReference apply(final MutableTypeParameterDeclaration it) {
                      return context.newTypeReference(it);
                    }
                  };
                  Iterable<TypeReference> _map = IterableExtensions.map(_typeParameters, _function);
                  List<TypeReference> _list = IterableExtensions.<TypeReference>toList(_map);
                  final TypeReference type = context.newTypeReference(_type_1, ((TypeReference[])Conversions.unwrapArray(_list, TypeReference.class)));
                  String _simpleName = it.getSimpleName();
                  injected.addParameter(_simpleName, type);
                }
              };
              IterableExtensions.forEach(_parameters, _function_1);
              TypeReference _returnType = m.getReturnType();
              Type _type = _returnType.getType();
              Iterable<? extends MutableTypeParameterDeclaration> _typeParameters_1 = injected.getTypeParameters();
              final Function1<MutableTypeParameterDeclaration, TypeReference> _function_2 = new Function1<MutableTypeParameterDeclaration, TypeReference>() {
                public TypeReference apply(final MutableTypeParameterDeclaration it) {
                  return context.newTypeReference(it);
                }
              };
              Iterable<TypeReference> _map = IterableExtensions.map(_typeParameters_1, _function_2);
              List<TypeReference> _list = IterableExtensions.<TypeReference>toList(_map);
              TypeReference _newTypeReference = context.newTypeReference(_type, ((TypeReference[])Conversions.unwrapArray(_list, TypeReference.class)));
              injected.setReturnType(_newTypeReference);
              Iterable<? extends TypeReference> _exceptions = m.getExceptions();
              injected.setExceptions(((TypeReference[])Conversions.unwrapArray(_exceptions, TypeReference.class)));
              final CompilationStrategy _function_3 = new CompilationStrategy() {
                public CharSequence compile(final CompilationStrategy.CompilationContext it) {
                  StringConcatenation _builder = new StringConcatenation();
                  {
                    TypeReference _returnType = m.getReturnType();
                    boolean _isVoid = _returnType.isVoid();
                    boolean _not = (!_isVoid);
                    if (_not) {
                      _builder.append("return");
                      _builder.newLine();
                    }
                  }
                  _builder.append(field, "");
                  _builder.append(".");
                  String _simpleName = m.getSimpleName();
                  _builder.append(_simpleName, "");
                  _builder.append("(");
                  _builder.newLineIfNotEmpty();
                  {
                    Iterable<? extends ParameterDeclaration> _parameters = m.getParameters();
                    boolean _hasElements = false;
                    for(final ParameterDeclaration p : _parameters) {
                      if (!_hasElements) {
                        _hasElements = true;
                      } else {
                        _builder.appendImmediate(",", "");
                      }
                      String _simpleName_1 = p.getSimpleName();
                      _builder.append(_simpleName_1, "");
                    }
                  }
                  _builder.newLineIfNotEmpty();
                  _builder.append(");");
                  _builder.newLine();
                  return _builder;
                }
              };
              injected.setBody(_function_3);
            }
          };
          annotatedClass.addMethod(_simpleName, _function_1);
        }
      }
    };
    IterableExtensions.forEach(methods, _function_1);
  }
}
