package org.erlide.annotations;

import java.util.function.Consumer;

import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.MutableFieldDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableParameterDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableTypeDeclaration;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend.lib.macro.declaration.Visibility;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

@SuppressWarnings("all")
public abstract class MethodMemoizer {
    @Extension
    protected final TransformationContext context;

    protected final MutableMethodDeclaration method;

    private final int index;

    public MethodMemoizer(final MutableMethodDeclaration method,
            final TransformationContext context, final int index) {
        this.method = method;
        this.context = context;
        this.index = index;
    }

    public final MutableMethodDeclaration generate() {
        MutableMethodDeclaration _xblockexpression = null;
        {
            final MutableTypeDeclaration _declaringType = method.getDeclaringType();
            final Procedure1<MutableTypeDeclaration> _function = (
                    final MutableTypeDeclaration it) -> {
                final Procedure1<MutableMethodDeclaration> _function_1 = (
                        final MutableMethodDeclaration init) -> {
                    init.setStatic(method.isStatic());
                    init.setVisibility(Visibility.PRIVATE);
                    init.setReturnType(wrappedReturnType());
                    final Consumer<MutableParameterDeclaration> _function_2 = (
                            final MutableParameterDeclaration it_1) -> {
                        init.addParameter(it_1.getSimpleName(), it_1.getType());
                    };
                    method.getParameters().forEach(_function_2);
                    init.setExceptions((TypeReference[]) Conversions
                            .unwrapArray(method.getExceptions(), TypeReference.class));
                    init.setBody(method.getBody());
                };
                it.addMethod(initMethodName(), _function_1);
                final Procedure1<MutableFieldDeclaration> _function_2 = (
                        final MutableFieldDeclaration it_1) -> {
                    it_1.setStatic(method.isStatic());
                    it_1.setType(cacheFieldType());
                    final CompilationStrategy _function_3 = (
                            final CompilationStrategy.CompilationContext it_2) -> {
                        return cacheFieldInit(it_2);
                    };
                    it_1.setInitializer(_function_3);
                };
                it.addField(cacheFieldName(), _function_2);
            };
            ObjectExtensions.<MutableTypeDeclaration> operator_doubleArrow(_declaringType,
                    _function);
            final Procedure1<MutableMethodDeclaration> _function_1 = (
                    final MutableMethodDeclaration it) -> {
                final CompilationStrategy _function_2 = (
                        final CompilationStrategy.CompilationContext it_1) -> {
                    return cacheCall(it_1);
                };
                it.setBody(_function_2);
                it.setReturnType(wrappedReturnType());
            };
            _xblockexpression = ObjectExtensions
                    .<MutableMethodDeclaration> operator_doubleArrow(method, _function_1);
        }
        return _xblockexpression;
    }

    protected final TypeReference wrappedReturnType() {
        return method.getReturnType().getWrapperIfPrimitive();
    }

    protected final String initMethodName() {
        final String _simpleName = method.getSimpleName();
        return _simpleName + "_init";
    }

    protected final String cacheFieldName() {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("cache");
        _builder.append(index);
        _builder.append("_");
        final String _simpleName = method.getSimpleName();
        _builder.append(_simpleName);
        return _builder.toString();
    }

    protected abstract CharSequence cacheCall(
            final CompilationStrategy.CompilationContext context);

    protected abstract TypeReference cacheFieldType();

    protected abstract CharSequence cacheFieldInit(
            final CompilationStrategy.CompilationContext context);
}
