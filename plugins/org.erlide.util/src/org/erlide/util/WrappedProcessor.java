package org.erlide.util;

import java.util.List;

import org.eclipse.xtend.lib.macro.AbstractClassProcessor;
import org.eclipse.xtend.lib.macro.RegisterGlobalsContext;
import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.ClassDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableClassDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableConstructorDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableFieldDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend.lib.macro.declaration.Visibility;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

import com.google.common.base.Objects;

@SuppressWarnings("all")
public class WrappedProcessor extends AbstractClassProcessor {
    @Override
    public void doRegisterGlobals(final ClassDeclaration annotatedClass,
            @Extension final RegisterGlobalsContext context) {
    }

    @Override
    public void doTransform(final MutableClassDeclaration annotatedClass,
            @Extension final TransformationContext context) {
        final boolean is_cached = IterableExtensions.head(annotatedClass.getAnnotations())
                .getBooleanValue("cached");
        final boolean is_synchronized = IterableExtensions
                .head(annotatedClass.getAnnotations()).getBooleanValue("synch");
        final Iterable<? extends TypeReference> intfs = annotatedClass
                .getImplementedInterfaces();
        final int _size = IterableExtensions.size(intfs);
        final boolean _notEquals = _size != 1;
        if (_notEquals) {
            context.addError(annotatedClass,
                    "Only one interface must be specified for a wrapped class");
        }
        final String _simpleName = IterableExtensions.head(intfs).getType()
                .getSimpleName();
        final boolean _notEquals_1 = !Objects.equal(_simpleName, "Wrapper");
        if (_notEquals_1) {
            final String _simpleName_1 = IterableExtensions.head(intfs).getType()
                    .getSimpleName();
            final String _plus = "Wrapped classes must implement Wrapper<T> "
                    + _simpleName_1;
            context.addError(annotatedClass, _plus);
        }
        final List<TypeReference> typeArgs = IterableExtensions.head(intfs)
                .getActualTypeArguments();
        final TypeReference argType0 = IterableExtensions.<TypeReference> head(typeArgs);
        final TypeReference argType = argType0.getPrimitiveIfWrapper();
        annotatedClass.setFinal(true);
        final Procedure1<MutableFieldDeclaration> _function = (
                final MutableFieldDeclaration it) -> {
            it.setFinal(true);
            it.setType(argType);
        };
        annotatedClass.addField("value", _function);
        if (is_cached) {
            final Procedure1<MutableFieldDeclaration> _function_1 = (
                    final MutableFieldDeclaration it) -> {
                it.setStatic(true);
                it.setType(context.newTypeReference(WeakPool.class, argType,
                        context.newTypeReference(annotatedClass)));
                final StringConcatenationClient _client = new StringConcatenationClient() {
                    @Override
                    protected void appendTo(
                            final StringConcatenationClient.TargetStringConcatenation _builder) {
                        _builder.append("new WeakPool<>()");
                        _builder.newLine();
                    }
                };
                it.setInitializer(_client);
            };
            annotatedClass.addField("cache", _function_1);
        }
        final Procedure1<MutableConstructorDeclaration> _function_2 = (
                final MutableConstructorDeclaration it) -> {
            it.setVisibility(Visibility.PRIVATE);
            it.addParameter("value", argType);
            final StringConcatenationClient _client = new StringConcatenationClient() {
                @Override
                protected void appendTo(
                        final StringConcatenationClient.TargetStringConcatenation _builder) {
                    _builder.append("this.value = value;");
                    _builder.newLine();
                }
            };
            it.setBody(_client);
        };
        annotatedClass.addConstructor(_function_2);
        final Procedure1<MutableMethodDeclaration> _function_3 = (
                final MutableMethodDeclaration it) -> {
            it.setStatic(true);
            it.setSynchronized(is_synchronized);
            it.addParameter("value", argType);
            it.setReturnType(context.newSelfTypeReference(annotatedClass));
            final StringConcatenationClient _client = new StringConcatenationClient() {
                @Override
                protected void appendTo(
                        final StringConcatenationClient.TargetStringConcatenation _builder) {
                    {
                        final boolean _isPrimitive = argType.isPrimitive();
                        final boolean _not = !_isPrimitive;
                        if (_not) {
                            _builder.append("if (value == null)");
                            _builder.newLine();
                            _builder.append("    ");
                            _builder.append(
                                    "throw new NullPointerException(\"Wrapped value must not be null\");");
                            _builder.newLine();
                        }
                    }
                    {
                        if (is_cached) {
                            final TypeReference _newSelfTypeReference = context
                                    .newSelfTypeReference(annotatedClass);
                            _builder.append(_newSelfTypeReference);
                            _builder.append(" v = cache.get(value);");
                            _builder.newLineIfNotEmpty();
                            _builder.append("if(v == null) {");
                            _builder.newLine();
                            _builder.append("    ");
                            _builder.append("v = new ");
                            final TypeReference _newSelfTypeReference_1 = context
                                    .newSelfTypeReference(annotatedClass);
                            _builder.append(_newSelfTypeReference_1, "    ");
                            _builder.append("(value);");
                            _builder.newLineIfNotEmpty();
                            _builder.append("    ");
                            _builder.append("cache.put(value, v);");
                            _builder.newLine();
                            _builder.append("}");
                            _builder.newLine();
                        } else {
                            final TypeReference _newSelfTypeReference_2 = context
                                    .newSelfTypeReference(annotatedClass);
                            _builder.append(_newSelfTypeReference_2);
                            _builder.append(" v = new ");
                            final TypeReference _newSelfTypeReference_3 = context
                                    .newSelfTypeReference(annotatedClass);
                            _builder.append(_newSelfTypeReference_3);
                            _builder.append("(value);");
                            _builder.newLineIfNotEmpty();
                        }
                    }
                    _builder.append("return v;");
                    _builder.newLine();
                }
            };
            it.setBody(_client);
        };
        annotatedClass.addMethod("of", _function_3);
        final Procedure1<MutableMethodDeclaration> _function_4 = (
                final MutableMethodDeclaration it) -> {
            it.setReturnType(argType);
            final StringConcatenationClient _client = new StringConcatenationClient() {
                @Override
                protected void appendTo(
                        final StringConcatenationClient.TargetStringConcatenation _builder) {
                    _builder.append("return value;");
                    _builder.newLine();
                }
            };
            it.setBody(_client);
        };
        annotatedClass.addMethod("value", _function_4);
        final Procedure1<MutableMethodDeclaration> _function_5 = (
                final MutableMethodDeclaration it) -> {
            it.addAnnotation(context.newAnnotationReference(Override.class));
            it.setReturnType(context.getString());
            final StringConcatenationClient _client = new StringConcatenationClient() {
                @Override
                protected void appendTo(
                        final StringConcatenationClient.TargetStringConcatenation _builder) {
                    _builder.append("return \"");
                    final String _simpleName = annotatedClass.getSimpleName();
                    _builder.append(_simpleName);
                    _builder.append("(\" + value + \")\";");
                    _builder.newLineIfNotEmpty();
                }
            };
            it.setBody(_client);
        };
        annotatedClass.addMethod("toString", _function_5);
        final Procedure1<MutableMethodDeclaration> _function_6 = (
                final MutableMethodDeclaration it) -> {
            it.addAnnotation(context.newAnnotationReference(Override.class));
            it.addParameter("other", context.newTypeReference(Object.class));
            it.setReturnType(context.newTypeReference(boolean.class));
            final StringConcatenationClient _client = new StringConcatenationClient() {
                @Override
                protected void appendTo(
                        final StringConcatenationClient.TargetStringConcatenation _builder) {
                    {
                        final boolean _isPrimitive = argType.isPrimitive();
                        if (_isPrimitive) {
                            _builder.append("return java.util.Objects.equals(value, ((");
                            final TypeReference _newSelfTypeReference = context
                                    .newSelfTypeReference(annotatedClass);
                            _builder.append(_newSelfTypeReference);
                            _builder.append(")other).value);");
                            _builder.newLineIfNotEmpty();
                        } else {
                            _builder.append("return java.util.Objects.equals(value, ((");
                            final TypeReference _newSelfTypeReference_1 = context
                                    .newSelfTypeReference(annotatedClass);
                            _builder.append(_newSelfTypeReference_1);
                            _builder.append(")other).value);");
                            _builder.newLineIfNotEmpty();
                        }
                    }
                }
            };
            it.setBody(_client);
        };
        annotatedClass.addMethod("equals", _function_6);
        final Procedure1<MutableMethodDeclaration> _function_7 = (
                final MutableMethodDeclaration it) -> {
            it.addAnnotation(context.newAnnotationReference(Override.class));
            it.setReturnType(context.newTypeReference(int.class));
            final StringConcatenationClient _client = new StringConcatenationClient() {
                @Override
                protected void appendTo(
                        final StringConcatenationClient.TargetStringConcatenation _builder) {
                    {
                        final boolean _isPrimitive = argType.isPrimitive();
                        if (_isPrimitive) {
                            _builder.append("return ");
                            final String _simpleName = argType0.getSimpleName();
                            _builder.append(_simpleName);
                            _builder.append(".hashCode(value);");
                            _builder.newLineIfNotEmpty();
                        } else {
                            _builder.append("return value.hashCode();");
                            _builder.newLine();
                        }
                    }
                }
            };
            it.setBody(_client);
        };
        annotatedClass.addMethod("hashCode", _function_7);
    }
}
