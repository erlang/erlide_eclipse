package org.erlide.annotations;

import com.google.common.base.Objects;
import com.google.common.collect.Iterables;
import java.util.List;
import java.util.Set;
import org.eclipse.xtend.lib.macro.RegisterGlobalsContext;
import org.eclipse.xtend.lib.macro.RegisterGlobalsParticipant;
import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.TransformationParticipant;
import org.eclipse.xtend.lib.macro.declaration.ClassDeclaration;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.CompilationUnit;
import org.eclipse.xtend.lib.macro.declaration.Element;
import org.eclipse.xtend.lib.macro.declaration.MemberDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableClassDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableParameterDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableTypeDeclaration;
import org.eclipse.xtend.lib.macro.declaration.ParameterDeclaration;
import org.eclipse.xtend.lib.macro.declaration.Type;
import org.eclipse.xtend.lib.macro.declaration.TypeDeclaration;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend.lib.macro.declaration.Visibility;
import org.eclipse.xtend.lib.macro.expression.Expression;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

@SuppressWarnings("all")
public class VisitableProcessor implements RegisterGlobalsParticipant<TypeDeclaration>, TransformationParticipant<MutableTypeDeclaration> {
  public void doRegisterGlobals(final List<? extends TypeDeclaration> types, final RegisterGlobalsContext context) {
    final Procedure1<TypeDeclaration> _function = new Procedure1<TypeDeclaration>() {
      public void apply(final TypeDeclaration it) {
        Iterable<MethodDeclaration> _acceptMethods = VisitableProcessor.this.getAcceptMethods(it);
        final Procedure1<MethodDeclaration> _function = new Procedure1<MethodDeclaration>() {
          public void apply(final MethodDeclaration it) {
            String _visitorName = VisitableProcessor.this.getVisitorName(it);
            context.registerClass(_visitorName);
          }
        };
        IterableExtensions.<MethodDeclaration>forEach(_acceptMethods, _function);
      }
    };
    IterableExtensions.forEach(types, _function);
  }
  
  private Iterable<MethodDeclaration> getAcceptMethods(final TypeDeclaration it) {
    Iterable<? extends MemberDeclaration> _declaredMembers = it.getDeclaredMembers();
    Iterable<MethodDeclaration> _filter = Iterables.<MethodDeclaration>filter(_declaredMembers, MethodDeclaration.class);
    final Function1<MethodDeclaration, Boolean> _function = new Function1<MethodDeclaration, Boolean>() {
      public Boolean apply(final MethodDeclaration it) {
        boolean _and = false;
        String _simpleName = it.getSimpleName();
        boolean _startsWith = _simpleName.startsWith("accept");
        if (!_startsWith) {
          _and = false;
        } else {
          Iterable<? extends ParameterDeclaration> _parameters = it.getParameters();
          int _size = IterableExtensions.size(_parameters);
          boolean _greaterEqualsThan = (_size >= 1);
          _and = _greaterEqualsThan;
        }
        return Boolean.valueOf(_and);
      }
    };
    return IterableExtensions.<MethodDeclaration>filter(_filter, _function);
  }
  
  private String getVisitorName(final MethodDeclaration it) {
    Iterable<? extends ParameterDeclaration> _parameters = it.getParameters();
    ParameterDeclaration _head = IterableExtensions.head(_parameters);
    TypeReference _type = _head.getType();
    Type _type_1 = _type.getType();
    return _type_1.getQualifiedName();
  }
  
  public void doTransform(final List<? extends MutableTypeDeclaration> types, @Extension final TransformationContext context) {
    final Procedure1<MutableTypeDeclaration> _function = new Procedure1<MutableTypeDeclaration>() {
      public void apply(final MutableTypeDeclaration root) {
        TypeReference _newTypeReference = context.newTypeReference(root);
        CompilationUnit _compilationUnit = root.getCompilationUnit();
        final Set<ClassDeclaration> allInheritors = VisitableProcessor.this.getInheritorsDeclaredIn(_newTypeReference, _compilationUnit, context);
        Iterable<MethodDeclaration> _acceptMethods = VisitableProcessor.this.getAcceptMethods(root);
        final Procedure1<MethodDeclaration> _function = new Procedure1<MethodDeclaration>() {
          public void apply(final MethodDeclaration method) {
            String _visitorName = VisitableProcessor.this.getVisitorName(method);
            final MutableClassDeclaration visitor = context.findClass(_visitorName);
            VisitableProcessor.this.addVisitMethods(visitor, allInheritors, method, context);
            final Function1<ClassDeclaration, MutableClassDeclaration> _function = new Function1<ClassDeclaration, MutableClassDeclaration>() {
              public MutableClassDeclaration apply(final ClassDeclaration it) {
                Element _primaryGeneratedJavaElement = context.getPrimaryGeneratedJavaElement(it);
                return ((MutableClassDeclaration) _primaryGeneratedJavaElement);
              }
            };
            Iterable<MutableClassDeclaration> _map = IterableExtensions.<ClassDeclaration, MutableClassDeclaration>map(allInheritors, _function);
            final Procedure1<MutableClassDeclaration> _function_1 = new Procedure1<MutableClassDeclaration>() {
              public void apply(final MutableClassDeclaration it) {
                String _simpleName = method.getSimpleName();
                Iterable<? extends ParameterDeclaration> _parameters = method.getParameters();
                final Function1<ParameterDeclaration, TypeReference> _function = new Function1<ParameterDeclaration, TypeReference>() {
                  public TypeReference apply(final ParameterDeclaration it) {
                    return it.getType();
                  }
                };
                Iterable<TypeReference> _map = IterableExtensions.map(_parameters, _function);
                final MutableMethodDeclaration existingMethod = it.findDeclaredMethod(_simpleName, ((TypeReference[])Conversions.unwrapArray(_map, TypeReference.class)));
                boolean _tripleEquals = (existingMethod == null);
                if (_tripleEquals) {
                  String _simpleName_1 = method.getSimpleName();
                  final Procedure1<MutableMethodDeclaration> _function_1 = new Procedure1<MutableMethodDeclaration>() {
                    public void apply(final MutableMethodDeclaration newMethod) {
                      TypeReference _returnType = method.getReturnType();
                      newMethod.setReturnType(_returnType);
                      Iterable<? extends ParameterDeclaration> _parameters = method.getParameters();
                      final Procedure1<ParameterDeclaration> _function = new Procedure1<ParameterDeclaration>() {
                        public void apply(final ParameterDeclaration it) {
                          String _simpleName = it.getSimpleName();
                          TypeReference _type = it.getType();
                          newMethod.addParameter(_simpleName, _type);
                        }
                      };
                      IterableExtensions.forEach(_parameters, _function);
                      final CompilationStrategy _function_1 = new CompilationStrategy() {
                        public CharSequence compile(final CompilationStrategy.CompilationContext it) {
                          return VisitableProcessor.this.getAcceptMethodBody(newMethod);
                        }
                      };
                      newMethod.setBody(_function_1);
                    }
                  };
                  it.addMethod(_simpleName_1, _function_1);
                } else {
                  final Procedure1<MutableMethodDeclaration> _function_2 = new Procedure1<MutableMethodDeclaration>() {
                    public void apply(final MutableMethodDeclaration it) {
                      MutableTypeDeclaration _declaringType = it.getDeclaringType();
                      boolean _equals = Objects.equal(root, _declaringType);
                      if (_equals) {
                        it.setAbstract(false);
                        final CompilationStrategy _function = new CompilationStrategy() {
                          public CharSequence compile(final CompilationStrategy.CompilationContext cu) {
                            return VisitableProcessor.this.getAcceptMethodBody(it);
                          }
                        };
                        it.setBody(_function);
                      }
                    }
                  };
                  ObjectExtensions.<MutableMethodDeclaration>operator_doubleArrow(existingMethod, _function_2);
                }
              }
            };
            IterableExtensions.<MutableClassDeclaration>forEach(_map, _function_1);
          }
        };
        IterableExtensions.<MethodDeclaration>forEach(_acceptMethods, _function);
      }
    };
    IterableExtensions.forEach(types, _function);
  }
  
  private CharSequence getAcceptMethodBody(final MutableMethodDeclaration it) {
    StringConcatenation _builder = new StringConcatenation();
    {
      TypeReference _returnType = it.getReturnType();
      boolean _isVoid = _returnType.isVoid();
      boolean _not = (!_isVoid);
      if (_not) {
        _builder.append("return ");
      }
    }
    Iterable<? extends MutableParameterDeclaration> _parameters = it.getParameters();
    MutableParameterDeclaration _head = IterableExtensions.head(_parameters);
    String _simpleName = _head.getSimpleName();
    _builder.append(_simpleName, "");
    _builder.append(".visit");
    MutableTypeDeclaration _declaringType = it.getDeclaringType();
    String _simpleName_1 = _declaringType.getSimpleName();
    _builder.append(_simpleName_1, "");
    _builder.append("(this");
    Iterable<? extends MutableParameterDeclaration> _parameters_1 = it.getParameters();
    List<? extends MutableParameterDeclaration> _list = IterableExtensions.toList(_parameters_1);
    Iterable<? extends MutableParameterDeclaration> _parameters_2 = it.getParameters();
    int _size = IterableExtensions.size(_parameters_2);
    List<? extends MutableParameterDeclaration> _subList = _list.subList(1, _size);
    final Function1<MutableParameterDeclaration, String> _function = new Function1<MutableParameterDeclaration, String>() {
      public String apply(final MutableParameterDeclaration it) {
        return it.getSimpleName();
      }
    };
    String _join = IterableExtensions.join(_subList, ", ", ", ", "", _function);
    _builder.append(_join, "");
    _builder.append(");");
    return _builder;
  }
  
  private void addVisitMethods(final MutableClassDeclaration clazz, final Set<? extends ClassDeclaration> inheritors, final MethodDeclaration original, @Extension final TransformationContext context) {
    clazz.setVisibility(Visibility.PUBLIC);
    final Procedure1<ClassDeclaration> _function = new Procedure1<ClassDeclaration>() {
      public void apply(final ClassDeclaration inheritor) {
        String _simpleName = inheritor.getSimpleName();
        String _plus = ("visit" + _simpleName);
        final Procedure1<MutableMethodDeclaration> _function = new Procedure1<MutableMethodDeclaration>() {
          public void apply(final MutableMethodDeclaration method) {
            TypeReference _returnType = original.getReturnType();
            method.setReturnType(_returnType);
            TypeReference _newTypeReference = context.newTypeReference(inheritor);
            method.addParameter("visitable", _newTypeReference);
            Iterable<? extends ParameterDeclaration> _parameters = original.getParameters();
            Iterable<? extends ParameterDeclaration> _drop = IterableExtensions.drop(_parameters, 1);
            final Procedure1<ParameterDeclaration> _function = new Procedure1<ParameterDeclaration>() {
              public void apply(final ParameterDeclaration it) {
                String _simpleName = it.getSimpleName();
                TypeReference _type = it.getType();
                method.addParameter(_simpleName, _type);
              }
            };
            IterableExtensions.forEach(_drop, _function);
            boolean _or = false;
            boolean _equals = Objects.equal(clazz, inheritor);
            if (_equals) {
              _or = true;
            } else {
              TypeReference _extendedClass = inheritor.getExtendedClass();
              boolean _tripleEquals = (_extendedClass == null);
              _or = _tripleEquals;
            }
            if (_or) {
              Expression _body = original.getBody();
              boolean _tripleNotEquals = (_body != null);
              if (_tripleNotEquals) {
                Expression _body_1 = original.getBody();
                method.setBody(_body_1);
              } else {
                final CompilationStrategy _function_1 = new CompilationStrategy() {
                  public CharSequence compile(final CompilationStrategy.CompilationContext it) {
                    StringConcatenation _builder = new StringConcatenation();
                    _builder.append("throw new IllegalStateException();");
                    return _builder;
                  }
                };
                method.setBody(_function_1);
              }
            } else {
              final CompilationStrategy _function_2 = new CompilationStrategy() {
                public CharSequence compile(final CompilationStrategy.CompilationContext it) {
                  StringConcatenation _builder = new StringConcatenation();
                  {
                    TypeReference _returnType = original.getReturnType();
                    boolean _isVoid = _returnType.isVoid();
                    boolean _not = (!_isVoid);
                    if (_not) {
                      _builder.append("return ");
                    }
                  }
                  _builder.append("visit");
                  TypeReference _extendedClass = inheritor.getExtendedClass();
                  Type _type = _extendedClass.getType();
                  String _simpleName = ((TypeDeclaration) _type).getSimpleName();
                  _builder.append(_simpleName, "");
                  _builder.append("(");
                  Iterable<? extends MutableParameterDeclaration> _parameters = method.getParameters();
                  final Function1<MutableParameterDeclaration, String> _function = new Function1<MutableParameterDeclaration, String>() {
                    public String apply(final MutableParameterDeclaration it) {
                      return it.getSimpleName();
                    }
                  };
                  Iterable<String> _map = IterableExtensions.map(_parameters, _function);
                  String _join = IterableExtensions.join(_map, ", ");
                  _builder.append(_join, "");
                  _builder.append(");");
                  return _builder;
                }
              };
              method.setBody(_function_2);
            }
          }
        };
        clazz.addMethod(_plus, _function);
      }
    };
    IterableExtensions.forEach(inheritors, _function);
  }
  
  private Set<ClassDeclaration> getInheritorsDeclaredIn(final TypeReference typeReference, final CompilationUnit it, @Extension final TransformationContext context) {
    Iterable<? extends TypeDeclaration> _sourceTypeDeclarations = it.getSourceTypeDeclarations();
    final Function1<TypeDeclaration, Boolean> _function = new Function1<TypeDeclaration, Boolean>() {
      public Boolean apply(final TypeDeclaration it) {
        TypeReference _newTypeReference = context.newTypeReference(it);
        return Boolean.valueOf(typeReference.isAssignableFrom(_newTypeReference));
      }
    };
    Iterable<? extends TypeDeclaration> _filter = IterableExtensions.filter(_sourceTypeDeclarations, _function);
    Iterable<ClassDeclaration> _filter_1 = Iterables.<ClassDeclaration>filter(_filter, ClassDeclaration.class);
    return IterableExtensions.<ClassDeclaration>toSet(_filter_1);
  }
}
