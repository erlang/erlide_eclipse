package org.erlide.annotations;

import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableParameterDeclaration;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;

@SuppressWarnings("all")
public class MultipleParameterMethodMemoizer extends ParametrizedMethodMemoizer {
    public MultipleParameterMethodMemoizer(final MutableMethodDeclaration method,
            final TransformationContext context, final int index) {
        super(method, context, index);
    }

    @Override
    protected CharSequence cacheKeyToParameters(
            @Extension final CompilationStrategy.CompilationContext context) {
        final Function1<MutableParameterDeclaration, CharSequence> _function = (
                final MutableParameterDeclaration it) -> {
            final StringConcatenation _builder = new StringConcatenation();
            _builder.append("(");
            final String _javaCode = context.toJavaCode(it.getType());
            _builder.append(_javaCode);
            _builder.append(") key.getParameters()[");
            final int _indexOf = IterableExtensions.toList(method.getParameters())
                    .indexOf(it);
            _builder.append(_indexOf);
            _builder.append("]");
            _builder.newLineIfNotEmpty();
            return _builder.toString();
        };
        return IterableExtensions.join(method.getParameters(), "", ",", "", _function);
    }

    @Override
    protected CharSequence parametersToCacheKey(
            @Extension final CompilationStrategy.CompilationContext context) {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("new ");
        final String _javaCode = context.toJavaCode(cacheKeyType());
        _builder.append(_javaCode);
        _builder.append("(");
        final Function1<MutableParameterDeclaration, CharSequence> _function = (
                final MutableParameterDeclaration it) -> {
            return it.getSimpleName();
        };
        final String _join = IterableExtensions.join(method.getParameters(), "", ",", "",
                _function);
        _builder.append(_join);
        _builder.append(")");
        _builder.newLineIfNotEmpty();
        return _builder;
    }

    @Override
    protected TypeReference cacheKeyType() {
        return context.newTypeReference(CacheKey.class);
    }
}
