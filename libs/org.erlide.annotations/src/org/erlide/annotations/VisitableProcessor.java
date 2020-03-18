package org.erlide.annotations;

import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

import org.eclipse.xtend.lib.macro.RegisterGlobalsContext;
import org.eclipse.xtend.lib.macro.RegisterGlobalsParticipant;
import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.TransformationParticipant;
import org.eclipse.xtend.lib.macro.declaration.ClassDeclaration;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.CompilationUnit;
import org.eclipse.xtend.lib.macro.declaration.Element;
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

import com.google.common.base.Objects;
import com.google.common.collect.Iterables;

@SuppressWarnings("all")
public class VisitableProcessor implements RegisterGlobalsParticipant<TypeDeclaration>,
        TransformationParticipant<MutableTypeDeclaration> {
    @Override
    public void doRegisterGlobals(final List<? extends TypeDeclaration> types,
            final RegisterGlobalsContext context) {
        final Consumer<TypeDeclaration> _function = (final TypeDeclaration it) -> {
            final Consumer<MethodDeclaration> _function_1 = (
                    final MethodDeclaration it_1) -> {
                context.registerClass(getVisitorName(it_1));
            };
            getAcceptMethods(it).forEach(_function_1);
        };
        types.forEach(_function);
    }

    private Iterable<MethodDeclaration> getAcceptMethods(final TypeDeclaration it) {
        final Function1<MethodDeclaration, Boolean> _function = (
                final MethodDeclaration it_1) -> {
            return Boolean.valueOf(it_1.getSimpleName().startsWith("accept")
                    && IterableExtensions.size(it_1.getParameters()) >= 1);
        };
        return IterableExtensions.<MethodDeclaration> filter(
                Iterables.<MethodDeclaration> filter(it.getDeclaredMembers(),
                        MethodDeclaration.class),
                _function);
    }

    private String getVisitorName(final MethodDeclaration it) {
        return IterableExtensions.head(it.getParameters()).getType().getType()
                .getQualifiedName();
    }

    @Override
    public void doTransform(final List<? extends MutableTypeDeclaration> types,
            @Extension final TransformationContext context) {
        final Consumer<MutableTypeDeclaration> _function = (
                final MutableTypeDeclaration root) -> {
            final Set<ClassDeclaration> allInheritors = getInheritorsDeclaredIn(
                    context.newTypeReference(root), root.getCompilationUnit(), context);
            final Consumer<MethodDeclaration> _function_1 = (
                    final MethodDeclaration method) -> {
                final MutableClassDeclaration visitor = context
                        .findClass(getVisitorName(method));
                addVisitMethods(visitor, allInheritors, method, context);
                final Function1<ClassDeclaration, MutableClassDeclaration> _function_2 = (
                        final ClassDeclaration it) -> {
                    final Element _primaryGeneratedJavaElement = context
                            .getPrimaryGeneratedJavaElement(it);
                    return (MutableClassDeclaration) _primaryGeneratedJavaElement;
                };
                final Consumer<MutableClassDeclaration> _function_3 = (
                        final MutableClassDeclaration it) -> {
                    final Function1<ParameterDeclaration, TypeReference> _function_4 = (
                            final ParameterDeclaration it_1) -> {
                        return it_1.getType();
                    };
                    final MutableMethodDeclaration existingMethod = it.findDeclaredMethod(
                            method.getSimpleName(),
                            (TypeReference[]) Conversions.unwrapArray(IterableExtensions
                                    .map(method.getParameters(), _function_4),
                                    TypeReference.class));
                    if (existingMethod == null) {
                        final Procedure1<MutableMethodDeclaration> _function_5 = (
                                final MutableMethodDeclaration newMethod) -> {
                            newMethod.setReturnType(method.getReturnType());
                            final Consumer<ParameterDeclaration> _function_6 = (
                                    final ParameterDeclaration it_1) -> {
                                newMethod.addParameter(it_1.getSimpleName(),
                                        it_1.getType());
                            };
                            method.getParameters().forEach(_function_6);
                            final CompilationStrategy _function_7 = (
                                    final CompilationStrategy.CompilationContext it_1) -> {
                                return getAcceptMethodBody(newMethod);
                            };
                            newMethod.setBody(_function_7);
                        };
                        it.addMethod(method.getSimpleName(), _function_5);
                    } else {
                        final Procedure1<MutableMethodDeclaration> _function_6 = (
                                final MutableMethodDeclaration it_1) -> {
                            final MutableTypeDeclaration _declaringType = it_1
                                    .getDeclaringType();
                            final boolean _equals = Objects.equal(root, _declaringType);
                            if (_equals) {
                                it_1.setAbstract(false);
                                final CompilationStrategy _function_7 = (
                                        final CompilationStrategy.CompilationContext cu) -> {
                                    return getAcceptMethodBody(it_1);
                                };
                                it_1.setBody(_function_7);
                            }
                        };
                        ObjectExtensions.<MutableMethodDeclaration> operator_doubleArrow(
                                existingMethod, _function_6);
                    }
                };
                IterableExtensions.<ClassDeclaration, MutableClassDeclaration> map(
                        allInheritors, _function_2).forEach(_function_3);
            };
            getAcceptMethods(root).forEach(_function_1);
        };
        types.forEach(_function);
    }

    private CharSequence getAcceptMethodBody(final MutableMethodDeclaration it) {
        final StringConcatenation _builder = new StringConcatenation();
        {
            final boolean _isVoid = it.getReturnType().isVoid();
            final boolean _not = !_isVoid;
            if (_not) {
                _builder.append("return ");
            }
        }
        final String _simpleName = IterableExtensions.head(it.getParameters())
                .getSimpleName();
        _builder.append(_simpleName);
        _builder.append(".visit");
        final String _simpleName_1 = it.getDeclaringType().getSimpleName();
        _builder.append(_simpleName_1);
        _builder.append("(this");
        final Function1<MutableParameterDeclaration, CharSequence> _function = (
                final MutableParameterDeclaration it_1) -> {
            return it_1.getSimpleName();
        };
        final String _join = IterableExtensions.join(
                IterableExtensions.toList(it.getParameters()).subList(1,
                        IterableExtensions.size(it.getParameters())),
                ", ", ", ", "", _function);
        _builder.append(_join);
        _builder.append(");");
        return _builder;
    }

    private void addVisitMethods(final MutableClassDeclaration clazz,
            final Set<? extends ClassDeclaration> inheritors,
            final MethodDeclaration original,
            @Extension final TransformationContext context) {
        clazz.setVisibility(Visibility.PUBLIC);
        final Consumer<ClassDeclaration> _function = (
                final ClassDeclaration inheritor) -> {
            final String _simpleName = inheritor.getSimpleName();
            final String _plus = "visit" + _simpleName;
            final Procedure1<MutableMethodDeclaration> _function_1 = (
                    final MutableMethodDeclaration method) -> {
                method.setReturnType(original.getReturnType());
                method.addParameter("visitable", context.newTypeReference(inheritor));
                final Consumer<ParameterDeclaration> _function_2 = (
                        final ParameterDeclaration it) -> {
                    method.addParameter(it.getSimpleName(), it.getType());
                };
                IterableExtensions.drop(original.getParameters(), 1).forEach(_function_2);
                if (Objects.equal(clazz, inheritor)
                        || inheritor.getExtendedClass() == null) {
                    final Expression _body = original.getBody();
                    final boolean _tripleNotEquals = _body != null;
                    if (_tripleNotEquals) {
                        method.setBody(original.getBody());
                    } else {
                        final CompilationStrategy _function_3 = (
                                final CompilationStrategy.CompilationContext it) -> {
                            final StringConcatenation _builder = new StringConcatenation();
                            _builder.append("throw new IllegalStateException();");
                            return _builder;
                        };
                        method.setBody(_function_3);
                    }
                } else {
                    final CompilationStrategy _function_4 = (
                            final CompilationStrategy.CompilationContext it) -> {
                        final StringConcatenation _builder = new StringConcatenation();
                        {
                            final boolean _isVoid = original.getReturnType().isVoid();
                            final boolean _not = !_isVoid;
                            if (_not) {
                                _builder.append("return ");
                            }
                        }
                        _builder.append("visit");
                        final Type _type = inheritor.getExtendedClass().getType();
                        final String _simpleName_1 = ((TypeDeclaration) _type)
                                .getSimpleName();
                        _builder.append(_simpleName_1);
                        _builder.append("(");
                        final Function1<MutableParameterDeclaration, String> _function_5 = (
                                final MutableParameterDeclaration it_1) -> {
                            return it_1.getSimpleName();
                        };
                        final String _join = IterableExtensions.join(IterableExtensions
                                .map(method.getParameters(), _function_5), ", ");
                        _builder.append(_join);
                        _builder.append(");");
                        return _builder;
                    };
                    method.setBody(_function_4);
                }
            };
            clazz.addMethod(_plus, _function_1);
        };
        inheritors.forEach(_function);
    }

    private Set<ClassDeclaration> getInheritorsDeclaredIn(
            final TypeReference typeReference, final CompilationUnit it,
            @Extension final TransformationContext context) {
        final Function1<TypeDeclaration, Boolean> _function = (
                final TypeDeclaration it_1) -> {
            return Boolean.valueOf(
                    typeReference.isAssignableFrom(context.newTypeReference(it_1)));
        };
        return IterableExtensions.<ClassDeclaration> toSet(
                Iterables.<ClassDeclaration> filter(IterableExtensions
                        .filter(it.getSourceTypeDeclarations(), _function),
                        ClassDeclaration.class));
    }
}
