package org.erlide.annotations;

import java.util.List;
import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableParameterDeclaration;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.annotations.CacheKey;
import org.erlide.annotations.ParametrizedMethodMemoizer;

@SuppressWarnings("all")
public class MultipleParameterMethodMemoizer extends ParametrizedMethodMemoizer {
  public MultipleParameterMethodMemoizer(final MutableMethodDeclaration method, final TransformationContext context, final int index) {
    super(method, context, index);
  }
  
  protected CharSequence cacheKeyToParameters(@Extension final CompilationStrategy.CompilationContext context) {
    Iterable<? extends MutableParameterDeclaration> _parameters = this.method.getParameters();
    final Function1<MutableParameterDeclaration, String> _function = new Function1<MutableParameterDeclaration, String>() {
      public String apply(final MutableParameterDeclaration it) {
        StringConcatenation _builder = new StringConcatenation();
        _builder.append("(");
        TypeReference _type = it.getType();
        String _javaCode = context.toJavaCode(_type);
        _builder.append(_javaCode, "");
        _builder.append(") key.getParameters()[");
        Iterable<? extends MutableParameterDeclaration> _parameters = MultipleParameterMethodMemoizer.this.method.getParameters();
        List<? extends MutableParameterDeclaration> _list = IterableExtensions.toList(_parameters);
        int _indexOf = _list.indexOf(it);
        _builder.append(_indexOf, "");
        _builder.append("]");
        _builder.newLineIfNotEmpty();
        return _builder.toString();
      }
    };
    return IterableExtensions.join(_parameters, "", ",", "", _function);
  }
  
  protected CharSequence parametersToCacheKey(@Extension final CompilationStrategy.CompilationContext context) {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("new ");
    TypeReference _cacheKeyType = this.cacheKeyType();
    String _javaCode = context.toJavaCode(_cacheKeyType);
    _builder.append(_javaCode, "");
    _builder.append("(");
    Iterable<? extends MutableParameterDeclaration> _parameters = this.method.getParameters();
    final Function1<MutableParameterDeclaration, String> _function = new Function1<MutableParameterDeclaration, String>() {
      public String apply(final MutableParameterDeclaration it) {
        return it.getSimpleName();
      }
    };
    String _join = IterableExtensions.join(_parameters, "", ",", "", _function);
    _builder.append(_join, "");
    _builder.append(")");
    _builder.newLineIfNotEmpty();
    return _builder;
  }
  
  protected TypeReference cacheKeyType() {
    return this.context.newTypeReference(CacheKey.class);
  }
}
