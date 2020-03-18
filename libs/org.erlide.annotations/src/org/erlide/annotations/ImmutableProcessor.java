package org.erlide.annotations;

import java.util.function.Consumer;

import org.eclipse.xtend.lib.macro.AbstractClassProcessor;
import org.eclipse.xtend.lib.macro.RegisterGlobalsContext;
import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.ClassDeclaration;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.MutableClassDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableConstructorDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableFieldDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.StringExtensions;

import com.google.common.base.Objects;

@SuppressWarnings("all")
public class ImmutableProcessor extends AbstractClassProcessor {
    @Override
    public void doRegisterGlobals(final ClassDeclaration cls,
            final RegisterGlobalsContext context) {
        context.registerClass(builderClassName(cls));
    }

    @Override
    public void doTransform(final MutableClassDeclaration cls,
            @Extension final TransformationContext context) {
        final TypeReference _extendedClass = cls.getExtendedClass();
        final TypeReference _object = context.getObject();
        final boolean _notEquals = !Objects.equal(_extendedClass, _object);
        if (_notEquals) {
            context.addError(cls, "Inheritance does not play well with immutability");
        }
        cls.setFinal(true);
        final MutableClassDeclaration _findClass = context
                .findClass(builderClassName(cls));
        final Procedure1<MutableClassDeclaration> _function = (
                final MutableClassDeclaration it) -> {
            it.setFinal(true);
            final Procedure1<MutableMethodDeclaration> _function_1 = (
                    final MutableMethodDeclaration it_1) -> {
                it_1.setReturnType(context.newTypeReference(cls));
                final CompilationStrategy _function_2 = (
                        final CompilationStrategy.CompilationContext it_2) -> {
                    final StringConcatenation _builder = new StringConcatenation();
                    _builder.append("return new ");
                    final String _simpleName = cls.getSimpleName();
                    _builder.append(_simpleName);
                    _builder.append("(");
                    final Function1<MutableFieldDeclaration, CharSequence> _function_3 = (
                            final MutableFieldDeclaration it_3) -> {
                        return it_3.getSimpleName();
                    };
                    final String _join = IterableExtensions.join(dataFields(cls), ",",
                            _function_3);
                    _builder.append(_join);
                    _builder.append(");");
                    _builder.newLineIfNotEmpty();
                    return _builder;
                };
                it_1.setBody(_function_2);
            };
            it.addMethod("build", _function_1);
            final Consumer<MutableFieldDeclaration> _function_2 = (
                    final MutableFieldDeclaration field) -> {
                final Procedure1<MutableMethodDeclaration> _function_3 = (
                        final MutableMethodDeclaration it_1) -> {
                    it_1.addParameter(field.getSimpleName(), field.getType());
                    it_1.setReturnType(context.newTypeReference(builderClassName(cls)));
                    final CompilationStrategy _function_4 = (
                            final CompilationStrategy.CompilationContext it_2) -> {
                        final StringConcatenation _builder = new StringConcatenation();
                        _builder.append("this.");
                        final String _simpleName = field.getSimpleName();
                        _builder.append(_simpleName);
                        _builder.append(" = ");
                        final String _simpleName_1 = field.getSimpleName();
                        _builder.append(_simpleName_1);
                        _builder.append(";");
                        _builder.newLineIfNotEmpty();
                        _builder.append("return this;");
                        _builder.newLine();
                        return _builder;
                    };
                    it_1.setBody(_function_4);
                };
                it.addMethod(field.getSimpleName(), _function_3);
                final Procedure1<MutableFieldDeclaration> _function_4 = (
                        final MutableFieldDeclaration it_1) -> {
                    it_1.setType(field.getType());
                };
                it.addField(field.getSimpleName(), _function_4);
            };
            dataFields(cls).forEach(_function_2);
        };
        final MutableClassDeclaration builder = ObjectExtensions
                .<MutableClassDeclaration> operator_doubleArrow(_findClass, _function);
        final Procedure1<MutableMethodDeclaration> _function_1 = (
                final MutableMethodDeclaration it) -> {
            it.setStatic(true);
            it.setReturnType(context.newTypeReference(cls));
            it.addParameter("init", context.newTypeReference(Procedure1.class,
                    context.newTypeReference(builder)));
            final CompilationStrategy _function_2 = (
                    final CompilationStrategy.CompilationContext it_1) -> {
                final StringConcatenation _builder = new StringConcatenation();
                final String _builderClassName = builderClassName(cls);
                _builder.append(_builderClassName);
                _builder.append(" builder = builder();");
                _builder.newLineIfNotEmpty();
                _builder.append("init.apply(builder);");
                _builder.newLine();
                _builder.append("return builder.build();");
                _builder.newLine();
                return _builder;
            };
            it.setBody(_function_2);
        };
        cls.addMethod("build", _function_1);
        final Procedure1<MutableMethodDeclaration> _function_2 = (
                final MutableMethodDeclaration it) -> {
            it.setReturnType(context.newTypeReference(builderClassName(cls)));
            it.setStatic(true);
            final CompilationStrategy _function_3 = (
                    final CompilationStrategy.CompilationContext it_1) -> {
                final StringConcatenation _builder = new StringConcatenation();
                _builder.append("return new ");
                final String _builderClassName = builderClassName(cls);
                _builder.append(_builderClassName);
                _builder.append("();");
                _builder.newLineIfNotEmpty();
                return _builder;
            };
            it.setBody(_function_3);
        };
        cls.addMethod("builder", _function_2);
        final Procedure1<MutableConstructorDeclaration> _function_3 = (
                final MutableConstructorDeclaration it) -> {
            final Consumer<MutableFieldDeclaration> _function_4 = (
                    final MutableFieldDeclaration field) -> {
                it.addParameter(field.getSimpleName(), field.getType());
            };
            dataFields(cls).forEach(_function_4);
            final CompilationStrategy _function_5 = (
                    final CompilationStrategy.CompilationContext it_1) -> {
                final StringConcatenation _builder = new StringConcatenation();
                {
                    final Iterable<? extends MutableFieldDeclaration> _dataFields = dataFields(
                            cls);
                    for (final MutableFieldDeclaration p : _dataFields) {
                        _builder.append("this.");
                        final String _simpleName = p.getSimpleName();
                        _builder.append(_simpleName);
                        _builder.append(" = ");
                        final String _simpleName_1 = p.getSimpleName();
                        _builder.append(_simpleName_1);
                        _builder.append(";");
                        _builder.newLineIfNotEmpty();
                    }
                }
                return _builder;
            };
            it.setBody(_function_5);
        };
        cls.addConstructor(_function_3);
        final Consumer<MutableFieldDeclaration> _function_4 = (
                final MutableFieldDeclaration field) -> {
            final TypeReference fieldType = field.getType();
            String _xifexpression = null;
            if (Objects.equal(fieldType, context.getPrimitiveBoolean())
                    || Objects.equal(fieldType.getType().getSimpleName(), "Boolean")) {
                _xifexpression = "is";
            } else {
                _xifexpression = "get";
            }
            final String prefix = _xifexpression;
            final String _firstUpper = StringExtensions
                    .toFirstUpper(field.getSimpleName());
            final String _plus = prefix + _firstUpper;
            final Procedure1<MutableMethodDeclaration> _function_5 = (
                    final MutableMethodDeclaration it) -> {
                it.setReturnType(field.getType());
                final CompilationStrategy _function_6 = (
                        final CompilationStrategy.CompilationContext it_1) -> {
                    final StringConcatenation _builder = new StringConcatenation();
                    _builder.append("return ");
                    final String _simpleName = field.getSimpleName();
                    _builder.append(_simpleName);
                    _builder.append(";");
                    _builder.newLineIfNotEmpty();
                    return _builder;
                };
                it.setBody(_function_6);
            };
            cls.addMethod(_plus, _function_5);
        };
        dataFields(cls).forEach(_function_4);
        final Procedure1<MutableMethodDeclaration> _function_5 = (
                final MutableMethodDeclaration it) -> {
            it.setReturnType(context.getPrimitiveBoolean());
            it.addParameter("o", context.getObject());
            final Function1<MutableFieldDeclaration, CharSequence> _function_6 = (
                    final MutableFieldDeclaration it_1) -> {
                final StringConcatenation _builder = new StringConcatenation();
                final String _objects = objects();
                _builder.append(_objects);
                _builder.append(".equal(");
                final String _simpleName = it_1.getSimpleName();
                _builder.append(_simpleName);
                _builder.append(", other.");
                final String _simpleName_1 = it_1.getSimpleName();
                _builder.append(_simpleName_1);
                _builder.append(")");
                return _builder.toString();
            };
            final String result = IterableExtensions.join(dataFields(cls), "\n&& ",
                    _function_6);
            final CompilationStrategy _function_7 = (
                    final CompilationStrategy.CompilationContext it_1) -> {
                final StringConcatenation _builder = new StringConcatenation();
                _builder.append("if (o instanceof ");
                final String _simpleName = cls.getSimpleName();
                _builder.append(_simpleName);
                _builder.append(") {");
                _builder.newLineIfNotEmpty();
                _builder.append("  ");
                final String _simpleName_1 = cls.getSimpleName();
                _builder.append(_simpleName_1, "  ");
                _builder.append(" other = (");
                final String _simpleName_2 = cls.getSimpleName();
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
            };
            it.setBody(_function_7);
        };
        cls.addMethod("equals", _function_5);
        final Procedure1<MutableMethodDeclaration> _function_6 = (
                final MutableMethodDeclaration it) -> {
            it.setReturnType(context.getPrimitiveInt());
            final CompilationStrategy _function_7 = (
                    final CompilationStrategy.CompilationContext it_1) -> {
                final StringConcatenation _builder = new StringConcatenation();
                _builder.append("return ");
                final String _objects = objects();
                _builder.append(_objects);
                _builder.append(".hashCode(");
                final Function1<MutableFieldDeclaration, CharSequence> _function_8 = (
                        final MutableFieldDeclaration it_2) -> {
                    return it_2.getSimpleName();
                };
                final String _join = IterableExtensions.join(dataFields(cls), ",",
                        _function_8);
                _builder.append(_join);
                _builder.append(");");
                return _builder;
            };
            it.setBody(_function_7);
        };
        cls.addMethod("hashCode", _function_6);
        final Procedure1<MutableMethodDeclaration> _function_7 = (
                final MutableMethodDeclaration it) -> {
            it.setReturnType(context.getString());
            final CompilationStrategy _function_8 = (
                    final CompilationStrategy.CompilationContext it_1) -> {
                final StringConcatenation _builder = new StringConcatenation();
                _builder.append(
                        "return new org.eclipse.xtext.xbase.lib.util.ToStringHelper().toString(this);");
                return _builder;
            };
            it.setBody(_function_8);
        };
        cls.addMethod("toString", _function_7);
    }

    public Iterable<? extends MutableFieldDeclaration> dataFields(
            final MutableClassDeclaration cls) {
        final Function1<MutableFieldDeclaration, Boolean> _function = (
                final MutableFieldDeclaration it) -> {
            final boolean _isStatic = it.isStatic();
            return Boolean.valueOf(_isStatic == false);
        };
        return IterableExtensions.filter(cls.getDeclaredFields(), _function);
    }

    public String builderClassName(final ClassDeclaration cls) {
        final String _qualifiedName = cls.getQualifiedName();
        return _qualifiedName + "Builder";
    }

    public String objects() {
        return "com.google.common.base.Objects";
    }
}
