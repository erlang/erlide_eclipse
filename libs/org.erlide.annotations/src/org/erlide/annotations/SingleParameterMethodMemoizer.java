package org.erlide.annotations;

import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableParameterDeclaration;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.IterableExtensions;

@SuppressWarnings("all")
public class SingleParameterMethodMemoizer extends ParametrizedMethodMemoizer {
    public SingleParameterMethodMemoizer(final MutableMethodDeclaration method,
            final TransformationContext context, final int index) {
        super(method, context, index);
    }

    @Override
    protected CharSequence cacheKeyToParameters(
            final CompilationStrategy.CompilationContext context) {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("key");
        return _builder;
    }

    @Override
    protected CharSequence parametersToCacheKey(
            final CompilationStrategy.CompilationContext context) {
        return parameter().getSimpleName();
    }

    @Override
    protected TypeReference cacheKeyType() {
        return parameter().getType().getWrapperIfPrimitive();
    }

    private MutableParameterDeclaration parameter() {
        return IterableExtensions.head(method.getParameters());
    }
}
