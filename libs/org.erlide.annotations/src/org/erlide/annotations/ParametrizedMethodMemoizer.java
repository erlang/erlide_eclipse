package org.erlide.annotations;

import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.AnnotationReference;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Extension;

@SuppressWarnings("all")
public abstract class ParametrizedMethodMemoizer extends MethodMemoizer {
    public ParametrizedMethodMemoizer(final MutableMethodDeclaration method,
            final TransformationContext context, final int index) {
        super(method, context, index);
    }

    @Override
    protected final CharSequence cacheFieldInit(
            @Extension final CompilationStrategy.CompilationContext context) {
        CharSequence _xblockexpression = null;
        {
            final AnnotationReference memoizeAnnotation = method.findAnnotation(
                    this.context.newTypeReference(Memoize.class).getType());
            final Object cacheDuration = memoizeAnnotation.getValue("cacheDuration");
            final Object maxSize = memoizeAnnotation.getValue("maxSize");
            final StringConcatenation _builder = new StringConcatenation();
            _builder.append("com.google.common.cache.CacheBuilder.newBuilder()");
            _builder.newLine();
            _builder.append(".expireAfterAccess(");
            _builder.append(cacheDuration);
            _builder.append(", ");
            final String _javaCode = context.toJavaCode(
                    this.context.newTypeReference("java.util.concurrent.TimeUnit"));
            _builder.append(_javaCode);
            _builder.append(".MILLISECONDS)");
            _builder.newLineIfNotEmpty();
            _builder.append(".maximumSize(");
            _builder.append(maxSize);
            _builder.append(")");
            _builder.newLineIfNotEmpty();
            _builder.append(".build(new com.google.common.cache.CacheLoader");
            final String _javaCode_1 = context.toJavaCode(cacheKeyType());
            _builder.append(_javaCode_1);
            _builder.append(", ");
            final String _javaCode_2 = context.toJavaCode(wrappedReturnType());
            _builder.append(_javaCode_2);
            _builder.append(">() {");
            _builder.newLineIfNotEmpty();
            _builder.append("  ");
            _builder.append("@Override");
            _builder.newLine();
            _builder.append("  ");
            _builder.append("public ");
            final String _javaCode_3 = context.toJavaCode(wrappedReturnType());
            _builder.append(_javaCode_3, "  ");
            _builder.append(" load(");
            final String _javaCode_4 = context.toJavaCode(cacheKeyType());
            _builder.append(_javaCode_4, "  ");
            _builder.append(" key) throws Exception {");
            _builder.newLineIfNotEmpty();
            _builder.append("    ");
            _builder.append("return ");
            final String _initMethodName = initMethodName();
            _builder.append(_initMethodName, "    ");
            _builder.append("(");
            final CharSequence _cacheKeyToParameters = cacheKeyToParameters(context);
            _builder.append(_cacheKeyToParameters, "    ");
            _builder.append(");");
            _builder.newLineIfNotEmpty();
            _builder.append("  ");
            _builder.append("}");
            _builder.newLine();
            _builder.append("})");
            _builder.newLine();
            _xblockexpression = _builder;
        }
        return _xblockexpression;
    }

    @Override
    protected final TypeReference cacheFieldType() {
        return context.newTypeReference("com.google.common.cache.LoadingCache",
                cacheKeyType(), wrappedReturnType());
    }

    @Override
    protected final CharSequence cacheCall(
            @Extension final CompilationStrategy.CompilationContext context) {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("try {");
        _builder.newLine();
        _builder.append("  ");
        _builder.append("return ");
        final String _cacheFieldName = cacheFieldName();
        _builder.append(_cacheFieldName, "  ");
        _builder.append(".get(");
        final CharSequence _parametersToCacheKey = parametersToCacheKey(context);
        _builder.append(_parametersToCacheKey, "  ");
        _builder.append(");");
        _builder.newLineIfNotEmpty();
        _builder.append("} catch (Throwable e) {");
        _builder.newLine();
        _builder.append("  ");
        _builder.append("throw ");
        final String _javaCode = context
                .toJavaCode(this.context.newTypeReference(Exceptions.class));
        _builder.append(_javaCode, "  ");
        _builder.append(".sneakyThrow(e.getCause());");
        _builder.newLineIfNotEmpty();
        _builder.append("}");
        _builder.newLine();
        return _builder;
    }

    protected abstract TypeReference cacheKeyType();

    protected abstract CharSequence parametersToCacheKey(
            final CompilationStrategy.CompilationContext context);

    protected abstract CharSequence cacheKeyToParameters(
            final CompilationStrategy.CompilationContext context);
}
