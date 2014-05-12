package org.erlide.annotations;

import com.google.common.base.Objects;
import org.eclipse.xtend.lib.macro.AbstractClassProcessor;
import org.eclipse.xtend.lib.macro.TransformationContext;
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

@SuppressWarnings("all")
public class EqualsAndHashCodeProcessor extends AbstractClassProcessor {
  public void doTransform(final MutableClassDeclaration annotatedClass, @Extension final TransformationContext context) {
    super.doTransform(annotatedClass, context);
    final Procedure1<MutableMethodDeclaration> _function = new Procedure1<MutableMethodDeclaration>() {
      public void apply(final MutableMethodDeclaration it) {
        TypeReference _newTypeReference = context.newTypeReference(Integer.TYPE);
        it.setReturnType(_newTypeReference);
        final CompilationStrategy _function = new CompilationStrategy() {
          public CharSequence compile(final CompilationStrategy.CompilationContext it) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("final int prime = 31;");
            _builder.newLine();
            _builder.append("int result = super.hashCode();");
            _builder.newLine();
            {
              Iterable<? extends MutableFieldDeclaration> _declaredFields = annotatedClass.getDeclaredFields();
              final Function1<MutableFieldDeclaration,Boolean> _function = new Function1<MutableFieldDeclaration,Boolean>() {
                public Boolean apply(final MutableFieldDeclaration it) {
                  boolean _isStatic = it.isStatic();
                  return Boolean.valueOf((!_isStatic));
                }
              };
              Iterable<? extends MutableFieldDeclaration> _filter = IterableExtensions.filter(_declaredFields, _function);
              for(final MutableFieldDeclaration f : _filter) {
                {
                  String _name = Boolean.TYPE.getName();
                  TypeReference _type = f.getType();
                  String _simpleName = _type.getSimpleName();
                  boolean _equals = Objects.equal(_name, _simpleName);
                  if (_equals) {
                    _builder.append("result = prime * result + (");
                    String _simpleName_1 = f.getSimpleName();
                    _builder.append(_simpleName_1, "");
                    _builder.append(" ? 1231 : 1237);");
                    _builder.newLineIfNotEmpty();
                  } else {
                    boolean _or = false;
                    boolean _or_1 = false;
                    boolean _or_2 = false;
                    String _name_1 = Integer.TYPE.getName();
                    TypeReference _type_1 = f.getType();
                    String _simpleName_2 = _type_1.getSimpleName();
                    boolean _equals_1 = Objects.equal(_name_1, _simpleName_2);
                    if (_equals_1) {
                      _or_2 = true;
                    } else {
                      String _name_2 = Character.TYPE.getName();
                      TypeReference _type_2 = f.getType();
                      String _simpleName_3 = _type_2.getSimpleName();
                      boolean _equals_2 = Objects.equal(_name_2, _simpleName_3);
                      _or_2 = _equals_2;
                    }
                    if (_or_2) {
                      _or_1 = true;
                    } else {
                      String _name_3 = Byte.TYPE.getName();
                      TypeReference _type_3 = f.getType();
                      String _simpleName_4 = _type_3.getSimpleName();
                      boolean _equals_3 = Objects.equal(_name_3, _simpleName_4);
                      _or_1 = _equals_3;
                    }
                    if (_or_1) {
                      _or = true;
                    } else {
                      String _name_4 = Short.TYPE.getName();
                      TypeReference _type_4 = f.getType();
                      String _simpleName_5 = _type_4.getSimpleName();
                      boolean _equals_4 = Objects.equal(_name_4, _simpleName_5);
                      _or = _equals_4;
                    }
                    if (_or) {
                      _builder.append("result = prime * result + ");
                      String _simpleName_6 = f.getSimpleName();
                      _builder.append(_simpleName_6, "");
                      _builder.append(";");
                      _builder.newLineIfNotEmpty();
                    } else {
                      String _name_5 = Long.TYPE.getName();
                      TypeReference _type_5 = f.getType();
                      String _simpleName_7 = _type_5.getSimpleName();
                      boolean _equals_5 = Objects.equal(_name_5, _simpleName_7);
                      if (_equals_5) {
                        _builder.append("result = prime * result + (int) (");
                        String _simpleName_8 = f.getSimpleName();
                        _builder.append(_simpleName_8, "");
                        _builder.append(" ^ (");
                        String _simpleName_9 = f.getSimpleName();
                        _builder.append(_simpleName_9, "");
                        _builder.append(" » 32));");
                        _builder.newLineIfNotEmpty();
                      } else {
                        String _name_6 = Float.TYPE.getName();
                        TypeReference _type_6 = f.getType();
                        String _simpleName_10 = _type_6.getSimpleName();
                        boolean _equals_6 = Objects.equal(_name_6, _simpleName_10);
                        if (_equals_6) {
                          _builder.append("result = prime * result + Float.floatToIntBits(");
                          String _simpleName_11 = f.getSimpleName();
                          _builder.append(_simpleName_11, "");
                          _builder.append(");");
                          _builder.newLineIfNotEmpty();
                        } else {
                          String _name_7 = Double.TYPE.getName();
                          TypeReference _type_7 = f.getType();
                          String _simpleName_12 = _type_7.getSimpleName();
                          boolean _equals_7 = Objects.equal(_name_7, _simpleName_12);
                          if (_equals_7) {
                            _builder.append("result = prime * result + (int) (Double.doubleToLongBits(");
                            String _simpleName_13 = f.getSimpleName();
                            _builder.append(_simpleName_13, "");
                            _builder.append(") ^ (Double.doubleToLongBits(");
                            String _simpleName_14 = f.getSimpleName();
                            _builder.append(_simpleName_14, "");
                            _builder.append(") » 32));");
                            _builder.newLineIfNotEmpty();
                          } else {
                            _builder.append("result = prime * result + ((");
                            String _simpleName_15 = f.getSimpleName();
                            _builder.append(_simpleName_15, "");
                            _builder.append("== null) ? 0 : ");
                            String _simpleName_16 = f.getSimpleName();
                            _builder.append(_simpleName_16, "");
                            _builder.append(".hashCode());");
                            _builder.newLineIfNotEmpty();
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
            _builder.append("return 0;");
            _builder.newLine();
            return _builder;
          }
        };
        it.setBody(_function);
      }
    };
    annotatedClass.addMethod("hashCode", _function);
    final Procedure1<MutableMethodDeclaration> _function_1 = new Procedure1<MutableMethodDeclaration>() {
      public void apply(final MutableMethodDeclaration it) {
        final CompilationStrategy _function = new CompilationStrategy() {
          public CharSequence compile(final CompilationStrategy.CompilationContext it) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("if (this == obj)");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("return true;");
            _builder.newLine();
            _builder.append("if (obj == null)");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("return false;");
            _builder.newLine();
            _builder.append("if (getClass() != obj.getClass())");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("return false;");
            _builder.newLine();
            return _builder;
          }
        };
        it.setBody(_function);
      }
    };
    annotatedClass.addMethod("equals", _function_1);
  }
}
