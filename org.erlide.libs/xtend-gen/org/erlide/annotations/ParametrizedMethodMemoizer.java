package org.erlide.annotations;

import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.AnnotationReference;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.Type;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.erlide.annotations.Memoize;
import org.erlide.annotations.MethodMemoizer;

@SuppressWarnings("all")
public abstract class ParametrizedMethodMemoizer extends MethodMemoizer {
  public ParametrizedMethodMemoizer(final MutableMethodDeclaration method, final TransformationContext context, final int index) {
    super(method, context, index);
  }
  
  protected final CharSequence cacheFieldInit(@Extension final CompilationStrategy.CompilationContext context) {
    CharSequence _xblockexpression = null;
    {
      TypeReference _newTypeReference = this.context.newTypeReference(Memoize.class);
      Type _type = _newTypeReference.getType();
      final AnnotationReference memoizeAnnotation = this.method.findAnnotation(_type);
      final Object cacheDuration = memoizeAnnotation.getValue("cacheDuration");
      final Object maxSize = memoizeAnnotation.getValue("maxSize");
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("com.google.common.cache.CacheBuilder.newBuilder()");
      _builder.newLine();
      _builder.append(".expireAfterAccess(");
      _builder.append(cacheDuration, "");
      _builder.append(", ");
      TypeReference _newTypeReference_1 = this.context.newTypeReference("java.util.concurrent.TimeUnit");
      String _javaCode = context.toJavaCode(_newTypeReference_1);
      _builder.append(_javaCode, "");
      _builder.append(".MILLISECONDS)");
      _builder.newLineIfNotEmpty();
      _builder.append(".maximumSize(");
      _builder.append(maxSize, "");
      _builder.append(")");
      _builder.newLineIfNotEmpty();
      _builder.append(".build(new com.google.common.cache.CacheLoader");
      TypeReference _cacheKeyType = this.cacheKeyType();
      String _javaCode_1 = context.toJavaCode(_cacheKeyType);
      _builder.append(_javaCode_1, "");
      _builder.append(", ");
      TypeReference _wrappedReturnType = this.wrappedReturnType();
      String _javaCode_2 = context.toJavaCode(_wrappedReturnType);
      _builder.append(_javaCode_2, "");
      _builder.append(">() {");
      _builder.newLineIfNotEmpty();
      _builder.append("  ");
      _builder.append("@Override");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("public ");
      TypeReference _wrappedReturnType_1 = this.wrappedReturnType();
      String _javaCode_3 = context.toJavaCode(_wrappedReturnType_1);
      _builder.append(_javaCode_3, "  ");
      _builder.append(" load(");
      TypeReference _cacheKeyType_1 = this.cacheKeyType();
      String _javaCode_4 = context.toJavaCode(_cacheKeyType_1);
      _builder.append(_javaCode_4, "  ");
      _builder.append(" key) throws Exception {");
      _builder.newLineIfNotEmpty();
      _builder.append("    ");
      _builder.append("return ");
      String _initMethodName = this.initMethodName();
      _builder.append(_initMethodName, "    ");
      _builder.append("(");
      CharSequence _cacheKeyToParameters = this.cacheKeyToParameters(context);
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
  
  protected final TypeReference cacheFieldType() {
    TypeReference _cacheKeyType = this.cacheKeyType();
    TypeReference _wrappedReturnType = this.wrappedReturnType();
    return this.context.newTypeReference(
      "com.google.common.cache.LoadingCache", _cacheKeyType, _wrappedReturnType);
  }
  
  protected final CharSequence cacheCall(@Extension final CompilationStrategy.CompilationContext context) {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("try {");
    _builder.newLine();
    _builder.append("  ");
    _builder.append("return ");
    String _cacheFieldName = this.cacheFieldName();
    _builder.append(_cacheFieldName, "  ");
    _builder.append(".get(");
    CharSequence _parametersToCacheKey = this.parametersToCacheKey(context);
    _builder.append(_parametersToCacheKey, "  ");
    _builder.append(");");
    _builder.newLineIfNotEmpty();
    _builder.append("} catch (Throwable e) {");
    _builder.newLine();
    _builder.append("  ");
    _builder.append("throw ");
    TypeReference _newTypeReference = this.context.newTypeReference(Exceptions.class);
    String _javaCode = context.toJavaCode(_newTypeReference);
    _builder.append(_javaCode, "  ");
    _builder.append(".sneakyThrow(e.getCause());");
    _builder.newLineIfNotEmpty();
    _builder.append("}");
    _builder.newLine();
    return _builder;
  }
  
  protected abstract TypeReference cacheKeyType();
  
  protected abstract CharSequence parametersToCacheKey(final CompilationStrategy.CompilationContext context);
  
  protected abstract CharSequence cacheKeyToParameters(final CompilationStrategy.CompilationContext context);
}
