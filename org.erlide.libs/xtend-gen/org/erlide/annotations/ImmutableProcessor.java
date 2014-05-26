package org.erlide.annotations;

import com.google.common.base.Objects;
import org.eclipse.xtend.lib.macro.AbstractClassProcessor;
import org.eclipse.xtend.lib.macro.RegisterGlobalsContext;
import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.ClassDeclaration;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.MutableClassDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableConstructorDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableFieldDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.Type;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.StringExtensions;

@SuppressWarnings("all")
public class ImmutableProcessor extends AbstractClassProcessor {
  public void doRegisterGlobals(final ClassDeclaration cls, final RegisterGlobalsContext context) {
    String _builderClassName = this.builderClassName(cls);
    context.registerClass(_builderClassName);
  }
  
  public void doTransform(final MutableClassDeclaration cls, @Extension final TransformationContext context) {
    TypeReference _extendedClass = cls.getExtendedClass();
    TypeReference _object = context.getObject();
    boolean _notEquals = (!Objects.equal(_extendedClass, _object));
    if (_notEquals) {
      context.addError(cls, "Inheritance does not play well with immutability");
    }
    cls.setFinal(true);
    String _builderClassName = this.builderClassName(cls);
    MutableClassDeclaration _findClass = context.findClass(_builderClassName);
    final Procedure1<MutableClassDeclaration> _function = new Procedure1<MutableClassDeclaration>() {
      public void apply(final MutableClassDeclaration it) {
        it.setFinal(true);
        final Procedure1<MutableMethodDeclaration> _function = new Procedure1<MutableMethodDeclaration>() {
          public void apply(final MutableMethodDeclaration it) {
            TypeReference _newTypeReference = context.newTypeReference(cls);
            it.setReturnType(_newTypeReference);
            final CompilationStrategy _function = new CompilationStrategy() {
              public CharSequence compile(final CompilationStrategy.CompilationContext it) {
                StringConcatenation _builder = new StringConcatenation();
                _builder.append("return new ");
                String _simpleName = cls.getSimpleName();
                _builder.append(_simpleName, "");
                _builder.append("(");
                Iterable<? extends MutableFieldDeclaration> _dataFields = ImmutableProcessor.this.dataFields(cls);
                final Function1<MutableFieldDeclaration, String> _function = new Function1<MutableFieldDeclaration, String>() {
                  public String apply(final MutableFieldDeclaration it) {
                    return it.getSimpleName();
                  }
                };
                String _join = IterableExtensions.join(_dataFields, ",", _function);
                _builder.append(_join, "");
                _builder.append(");");
                _builder.newLineIfNotEmpty();
                return _builder;
              }
            };
            it.setBody(_function);
          }
        };
        it.addMethod("build", _function);
        Iterable<? extends MutableFieldDeclaration> _dataFields = ImmutableProcessor.this.dataFields(cls);
        final Procedure1<MutableFieldDeclaration> _function_1 = new Procedure1<MutableFieldDeclaration>() {
          public void apply(final MutableFieldDeclaration field) {
            String _simpleName = field.getSimpleName();
            final Procedure1<MutableMethodDeclaration> _function = new Procedure1<MutableMethodDeclaration>() {
              public void apply(final MutableMethodDeclaration it) {
                String _simpleName = field.getSimpleName();
                TypeReference _type = field.getType();
                it.addParameter(_simpleName, _type);
                String _builderClassName = ImmutableProcessor.this.builderClassName(cls);
                TypeReference _newTypeReference = context.newTypeReference(_builderClassName);
                it.setReturnType(_newTypeReference);
                final CompilationStrategy _function = new CompilationStrategy() {
                  public CharSequence compile(final CompilationStrategy.CompilationContext it) {
                    StringConcatenation _builder = new StringConcatenation();
                    _builder.append("this.");
                    String _simpleName = field.getSimpleName();
                    _builder.append(_simpleName, "");
                    _builder.append(" = ");
                    String _simpleName_1 = field.getSimpleName();
                    _builder.append(_simpleName_1, "");
                    _builder.append(";");
                    _builder.newLineIfNotEmpty();
                    _builder.append("return this;");
                    _builder.newLine();
                    return _builder;
                  }
                };
                it.setBody(_function);
              }
            };
            it.addMethod(_simpleName, _function);
            String _simpleName_1 = field.getSimpleName();
            final Procedure1<MutableFieldDeclaration> _function_1 = new Procedure1<MutableFieldDeclaration>() {
              public void apply(final MutableFieldDeclaration it) {
                TypeReference _type = field.getType();
                it.setType(_type);
              }
            };
            it.addField(_simpleName_1, _function_1);
          }
        };
        IterableExtensions.forEach(_dataFields, _function_1);
      }
    };
    final MutableClassDeclaration builder = ObjectExtensions.<MutableClassDeclaration>operator_doubleArrow(_findClass, _function);
    final Procedure1<MutableMethodDeclaration> _function_1 = new Procedure1<MutableMethodDeclaration>() {
      public void apply(final MutableMethodDeclaration it) {
        it.setStatic(true);
        TypeReference _newTypeReference = context.newTypeReference(cls);
        it.setReturnType(_newTypeReference);
        TypeReference _newTypeReference_1 = context.newTypeReference(builder);
        TypeReference _newTypeReference_2 = context.newTypeReference(Procedure1.class, _newTypeReference_1);
        it.addParameter("init", _newTypeReference_2);
        final CompilationStrategy _function = new CompilationStrategy() {
          public CharSequence compile(final CompilationStrategy.CompilationContext it) {
            StringConcatenation _builder = new StringConcatenation();
            String _builderClassName = ImmutableProcessor.this.builderClassName(cls);
            _builder.append(_builderClassName, "");
            _builder.append(" builder = builder();");
            _builder.newLineIfNotEmpty();
            _builder.append("init.apply(builder);");
            _builder.newLine();
            _builder.append("return builder.build();");
            _builder.newLine();
            return _builder;
          }
        };
        it.setBody(_function);
      }
    };
    cls.addMethod("build", _function_1);
    final Procedure1<MutableMethodDeclaration> _function_2 = new Procedure1<MutableMethodDeclaration>() {
      public void apply(final MutableMethodDeclaration it) {
        String _builderClassName = ImmutableProcessor.this.builderClassName(cls);
        TypeReference _newTypeReference = context.newTypeReference(_builderClassName);
        it.setReturnType(_newTypeReference);
        it.setStatic(true);
        final CompilationStrategy _function = new CompilationStrategy() {
          public CharSequence compile(final CompilationStrategy.CompilationContext it) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("return new ");
            String _builderClassName = ImmutableProcessor.this.builderClassName(cls);
            _builder.append(_builderClassName, "");
            _builder.append("();");
            _builder.newLineIfNotEmpty();
            return _builder;
          }
        };
        it.setBody(_function);
      }
    };
    cls.addMethod("builder", _function_2);
    final Procedure1<MutableConstructorDeclaration> _function_3 = new Procedure1<MutableConstructorDeclaration>() {
      public void apply(final MutableConstructorDeclaration it) {
        Iterable<? extends MutableFieldDeclaration> _dataFields = ImmutableProcessor.this.dataFields(cls);
        final Procedure1<MutableFieldDeclaration> _function = new Procedure1<MutableFieldDeclaration>() {
          public void apply(final MutableFieldDeclaration field) {
            String _simpleName = field.getSimpleName();
            TypeReference _type = field.getType();
            it.addParameter(_simpleName, _type);
          }
        };
        IterableExtensions.forEach(_dataFields, _function);
        final CompilationStrategy _function_1 = new CompilationStrategy() {
          public CharSequence compile(final CompilationStrategy.CompilationContext it) {
            StringConcatenation _builder = new StringConcatenation();
            {
              Iterable<? extends MutableFieldDeclaration> _dataFields = ImmutableProcessor.this.dataFields(cls);
              for(final MutableFieldDeclaration p : _dataFields) {
                _builder.append("this.");
                String _simpleName = p.getSimpleName();
                _builder.append(_simpleName, "");
                _builder.append(" = ");
                String _simpleName_1 = p.getSimpleName();
                _builder.append(_simpleName_1, "");
                _builder.append(";");
                _builder.newLineIfNotEmpty();
              }
            }
            return _builder;
          }
        };
        it.setBody(_function_1);
      }
    };
    cls.addConstructor(_function_3);
    Iterable<? extends MutableFieldDeclaration> _dataFields = this.dataFields(cls);
    final Procedure1<MutableFieldDeclaration> _function_4 = new Procedure1<MutableFieldDeclaration>() {
      public void apply(final MutableFieldDeclaration field) {
        final TypeReference fieldType = field.getType();
        String _xifexpression = null;
        boolean _or = false;
        TypeReference _primitiveBoolean = context.getPrimitiveBoolean();
        boolean _equals = Objects.equal(fieldType, _primitiveBoolean);
        if (_equals) {
          _or = true;
        } else {
          Type _type = fieldType.getType();
          String _simpleName = _type.getSimpleName();
          boolean _equals_1 = Objects.equal(_simpleName, "Boolean");
          _or = _equals_1;
        }
        if (_or) {
          _xifexpression = "is";
        } else {
          _xifexpression = "get";
        }
        final String prefix = _xifexpression;
        String _simpleName_1 = field.getSimpleName();
        String _firstUpper = StringExtensions.toFirstUpper(_simpleName_1);
        String _plus = (prefix + _firstUpper);
        final Procedure1<MutableMethodDeclaration> _function = new Procedure1<MutableMethodDeclaration>() {
          public void apply(final MutableMethodDeclaration it) {
            TypeReference _type = field.getType();
            it.setReturnType(_type);
            final CompilationStrategy _function = new CompilationStrategy() {
              public CharSequence compile(final CompilationStrategy.CompilationContext it) {
                StringConcatenation _builder = new StringConcatenation();
                _builder.append("return ");
                String _simpleName = field.getSimpleName();
                _builder.append(_simpleName, "");
                _builder.append(";");
                _builder.newLineIfNotEmpty();
                return _builder;
              }
            };
            it.setBody(_function);
          }
        };
        cls.addMethod(_plus, _function);
      }
    };
    IterableExtensions.forEach(_dataFields, _function_4);
    final Procedure1<MutableMethodDeclaration> _function_5 = new Procedure1<MutableMethodDeclaration>() {
      public void apply(final MutableMethodDeclaration it) {
        TypeReference _primitiveBoolean = context.getPrimitiveBoolean();
        it.setReturnType(_primitiveBoolean);
        TypeReference _object = context.getObject();
        it.addParameter("o", _object);
        Iterable<? extends MutableFieldDeclaration> _dataFields = ImmutableProcessor.this.dataFields(cls);
        final Function1<MutableFieldDeclaration, String> _function = new Function1<MutableFieldDeclaration, String>() {
          public String apply(final MutableFieldDeclaration it) {
            StringConcatenation _builder = new StringConcatenation();
            String _objects = ImmutableProcessor.this.objects();
            _builder.append(_objects, "");
            _builder.append(".equal(");
            String _simpleName = it.getSimpleName();
            _builder.append(_simpleName, "");
            _builder.append(", other.");
            String _simpleName_1 = it.getSimpleName();
            _builder.append(_simpleName_1, "");
            _builder.append(")");
            return _builder.toString();
          }
        };
        final String result = IterableExtensions.join(_dataFields, "\n&& ", _function);
        final CompilationStrategy _function_1 = new CompilationStrategy() {
          public CharSequence compile(final CompilationStrategy.CompilationContext it) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("if (o instanceof ");
            String _simpleName = cls.getSimpleName();
            _builder.append(_simpleName, "");
            _builder.append(") {");
            _builder.newLineIfNotEmpty();
            _builder.append("  ");
            String _simpleName_1 = cls.getSimpleName();
            _builder.append(_simpleName_1, "  ");
            _builder.append(" other = (");
            String _simpleName_2 = cls.getSimpleName();
            _builder.append(_simpleName_2, "  ");
            _builder.append(") o;");
            _builder.newLineIfNotEmpty();
            _builder.append("  ");
            _builder.append("return ");
            _builder.append(result, "  ");
            _builder.append(";");
            _builder.newLineIfNotEmpty();
            _builder.append("}");
            _builder.newLine();
            _builder.append("return false;");
            _builder.newLine();
            return _builder;
          }
        };
        it.setBody(_function_1);
      }
    };
    cls.addMethod("equals", _function_5);
    final Procedure1<MutableMethodDeclaration> _function_6 = new Procedure1<MutableMethodDeclaration>() {
      public void apply(final MutableMethodDeclaration it) {
        TypeReference _primitiveInt = context.getPrimitiveInt();
        it.setReturnType(_primitiveInt);
        final CompilationStrategy _function = new CompilationStrategy() {
          public CharSequence compile(final CompilationStrategy.CompilationContext it) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("return ");
            String _objects = ImmutableProcessor.this.objects();
            _builder.append(_objects, "");
            _builder.append(".hashCode(");
            Iterable<? extends MutableFieldDeclaration> _dataFields = ImmutableProcessor.this.dataFields(cls);
            final Function1<MutableFieldDeclaration, String> _function = new Function1<MutableFieldDeclaration, String>() {
              public String apply(final MutableFieldDeclaration it) {
                return it.getSimpleName();
              }
            };
            String _join = IterableExtensions.join(_dataFields, ",", _function);
            _builder.append(_join, "");
            _builder.append(");");
            return _builder;
          }
        };
        it.setBody(_function);
      }
    };
    cls.addMethod("hashCode", _function_6);
    final Procedure1<MutableMethodDeclaration> _function_7 = new Procedure1<MutableMethodDeclaration>() {
      public void apply(final MutableMethodDeclaration it) {
        TypeReference _string = context.getString();
        it.setReturnType(_string);
        final CompilationStrategy _function = new CompilationStrategy() {
          public CharSequence compile(final CompilationStrategy.CompilationContext it) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("return new org.eclipse.xtext.xbase.lib.util.ToStringHelper().toString(this);");
            return _builder;
          }
        };
        it.setBody(_function);
      }
    };
    cls.addMethod("toString", _function_7);
  }
  
  public Iterable<? extends MutableFieldDeclaration> dataFields(final MutableClassDeclaration cls) {
    Iterable<? extends MutableFieldDeclaration> _declaredFields = cls.getDeclaredFields();
    final Function1<MutableFieldDeclaration, Boolean> _function = new Function1<MutableFieldDeclaration, Boolean>() {
      public Boolean apply(final MutableFieldDeclaration it) {
        boolean _isStatic = it.isStatic();
        return Boolean.valueOf((_isStatic == false));
      }
    };
    return IterableExtensions.filter(_declaredFields, _function);
  }
  
  public String builderClassName(final ClassDeclaration cls) {
    String _qualifiedName = cls.getQualifiedName();
    return (_qualifiedName + "Builder");
  }
  
  public String objects() {
    return "com.google.common.base.Objects";
  }
}
