package org.erlide.annotations;

import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableParameterDeclaration;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.annotations.ParametrizedMethodMemoizer;

@SuppressWarnings("all")
public class SingleParameterMethodMemoizer extends ParametrizedMethodMemoizer {
  public SingleParameterMethodMemoizer(final MutableMethodDeclaration method, final TransformationContext context, final int index) {
    super(method, context, index);
  }
  
  protected CharSequence cacheKeyToParameters(final CompilationStrategy.CompilationContext context) {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("key");
    return _builder;
  }
  
  protected CharSequence parametersToCacheKey(final CompilationStrategy.CompilationContext context) {
    MutableParameterDeclaration _parameter = this.parameter();
    return _parameter.getSimpleName();
  }
  
  protected TypeReference cacheKeyType() {
    MutableParameterDeclaration _parameter = this.parameter();
    TypeReference _type = _parameter.getType();
    return _type.getWrapperIfPrimitive();
  }
  
  private MutableParameterDeclaration parameter() {
    Iterable<? extends MutableParameterDeclaration> _parameters = this.method.getParameters();
    return IterableExtensions.head(_parameters);
  }
}
