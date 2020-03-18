/**
 * https://atlas.assembla.com/code/vmat/subversion/nodes/109/reves-ann/trunk/src/main/java/net/virtualmat/reves/Delegate.xtend
 */
package org.erlide.annotations;

import java.util.function.Consumer;

import org.eclipse.xtend.lib.macro.AbstractClassProcessor;
import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.AnnotationReference;
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

import com.google.common.base.Objects;

@SuppressWarnings("all")
public class DelegateParticipant extends AbstractClassProcessor {
    @Override
    public void doTransform(final MutableClassDeclaration annotatedClass,
            @Extension final TransformationContext context) {
        final Function1<AnnotationReference, Boolean> _function = (
                final AnnotationReference it) -> {
            final String _simpleName = it.getAnnotationTypeDeclaration().getSimpleName();
            final String _simpleName_1 = Delegate.class.getSimpleName();
            return Boolean.valueOf(Objects.equal(_simpleName, _simpleName_1));
        };
        final AnnotationReference annotation = IterableExtensions.head(
                IterableExtensions.filter(annotatedClass.getAnnotations(), _function));
        final String field = annotation.getStringValue("field");
        final Type _type = annotation.getClassValue("to").getType();
        final TypeDeclaration to = (TypeDeclaration) _type;
        process(to, field, annotatedClass, context);
    }

    public void process(final TypeDeclaration to, final String field,
            final MutableClassDeclaration annotatedClass,
            @Extension final TransformationContext context) {
        final Function1<MethodDeclaration, Boolean> _function = (
                final MethodDeclaration it) -> {
            final Visibility _visibility = it.getVisibility();
            return Boolean.valueOf(Objects.equal(_visibility, Visibility.PUBLIC));
        };
        final Iterable<? extends MethodDeclaration> methods = IterableExtensions
                .filter(to.getDeclaredMethods(), _function);
        final Consumer<MethodDeclaration> _function_1 = (final MethodDeclaration m) -> {
            final Function1<MutableMethodDeclaration, Boolean> _function_2 = (
                    final MutableMethodDeclaration it) -> {
                final String _simpleName = it.getSimpleName();
                final String _simpleName_1 = m.getSimpleName();
                return Boolean.valueOf(Objects.equal(_simpleName, _simpleName_1));
            };
            final MutableMethodDeclaration exists = IterableExtensions
                    .findFirst(annotatedClass.getDeclaredMethods(), _function_2);
            if (exists == null) {
                final Procedure1<MutableMethodDeclaration> _function_3 = (
                        final MutableMethodDeclaration injected) -> {
                    final Consumer<TypeParameterDeclaration> _function_4 = (
                            final TypeParameterDeclaration it) -> {
                        injected.addTypeParameter(it.getSimpleName(),
                                (TypeReference[]) Conversions.unwrapArray(
                                        IterableExtensions.toList(it.getUpperBounds()),
                                        TypeReference.class));
                    };
                    m.getTypeParameters().forEach(_function_4);
                    final Consumer<ParameterDeclaration> _function_5 = (
                            final ParameterDeclaration it) -> {
                        final Function1<MutableTypeParameterDeclaration, TypeReference> _function_6 = (
                                final MutableTypeParameterDeclaration it_1) -> {
                            return context.newTypeReference(it_1);
                        };
                        final TypeReference type = context.newTypeReference(
                                it.getType().getType(),
                                (TypeReference[]) Conversions.unwrapArray(
                                        IterableExtensions
                                                .<TypeReference> toList(IterableExtensions
                                                        .map(injected.getTypeParameters(),
                                                                _function_6)),
                                        TypeReference.class));
                        injected.addParameter(it.getSimpleName(), type);
                    };
                    m.getParameters().forEach(_function_5);
                    final Function1<MutableTypeParameterDeclaration, TypeReference> _function_6 = (
                            final MutableTypeParameterDeclaration it) -> {
                        return context.newTypeReference(it);
                    };
                    injected.setReturnType(context.newTypeReference(
                            m.getReturnType().getType(),
                            (TypeReference[]) Conversions.unwrapArray(IterableExtensions
                                    .<TypeReference> toList(IterableExtensions.map(
                                            injected.getTypeParameters(), _function_6)),
                                    TypeReference.class)));
                    injected.setExceptions((TypeReference[]) Conversions
                            .unwrapArray(m.getExceptions(), TypeReference.class));
                    final CompilationStrategy _function_7 = (
                            final CompilationStrategy.CompilationContext it) -> {
                        final StringConcatenation _builder = new StringConcatenation();
                        {
                            final boolean _isVoid = m.getReturnType().isVoid();
                            final boolean _not = !_isVoid;
                            if (_not) {
                                _builder.append("return");
                                _builder.newLine();
                            }
                        }
                        _builder.append(field);
                        _builder.append(".");
                        final String _simpleName = m.getSimpleName();
                        _builder.append(_simpleName);
                        _builder.append("(");
                        _builder.newLineIfNotEmpty();
                        {
                            final Iterable<? extends ParameterDeclaration> _parameters = m
                                    .getParameters();
                            boolean _hasElements = false;
                            for (final ParameterDeclaration p : _parameters) {
                                if (!_hasElements) {
                                    _hasElements = true;
                                } else {
                                    _builder.appendImmediate(",", "");
                                }
                                final String _simpleName_1 = p.getSimpleName();
                                _builder.append(_simpleName_1);
                            }
                        }
                        _builder.newLineIfNotEmpty();
                        _builder.append(");");
                        _builder.newLine();
                        return _builder;
                    };
                    injected.setBody(_function_7);
                };
                annotatedClass.addMethod(m.getSimpleName(), _function_3);
            }
        };
        methods.forEach(_function_1);
    }
}
