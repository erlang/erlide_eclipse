package org.erlide.annotations;

import java.util.List;
import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy.CompilationContext;
import org.eclipse.xtend.lib.macro.declaration.MutableFieldDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableParameterDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableTypeDeclaration;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend.lib.macro.declaration.Visibility;
import org.eclipse.xtend.lib.macro.expression.Expression;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

@SuppressWarnings("all")
public abstract class MethodMemoizer {
  @Extension
  protected final TransformationContext context;
  
  protected final MutableMethodDeclaration method;
  
  private final int index;
  
  public MethodMemoizer(final MutableMethodDeclaration method, final TransformationContext context, final int index) {
    this.method = method;
    this.context = context;
    this.index = index;
  }
  
  public final MutableMethodDeclaration generate() {
    MutableMethodDeclaration _xblockexpression = null;
    {
      MutableTypeDeclaration _declaringType = this.method.getDeclaringType();
      final Procedure1<MutableTypeDeclaration> _function = new Procedure1<MutableTypeDeclaration>() {
        public void apply(final MutableTypeDeclaration it) {
          String _initMethodName = MethodMemoizer.this.initMethodName();
          final Procedure1<MutableMethodDeclaration> _function = new Procedure1<MutableMethodDeclaration>() {
            public void apply(final MutableMethodDeclaration init) {
              boolean _isStatic = MethodMemoizer.this.method.isStatic();
              init.setStatic(_isStatic);
              init.setVisibility(Visibility.PRIVATE);
              TypeReference _wrappedReturnType = MethodMemoizer.this.wrappedReturnType();
              init.setReturnType(_wrappedReturnType);
              List<MutableParameterDeclaration> _parameters = MethodMemoizer.this.method.getParameters();
              final Procedure1<MutableParameterDeclaration> _function = new Procedure1<MutableParameterDeclaration>() {
                public void apply(final MutableParameterDeclaration it) {
                  String _simpleName = it.getSimpleName();
                  TypeReference _type = it.getType();
                  init.addParameter(_simpleName, _type);
                }
              };
              IterableExtensions.<MutableParameterDeclaration>forEach(_parameters, _function);
              List<TypeReference> _exceptions = MethodMemoizer.this.method.getExceptions();
              init.setExceptions(((TypeReference[])Conversions.unwrapArray(_exceptions, TypeReference.class)));
              Expression _body = MethodMemoizer.this.method.getBody();
              init.setBody(_body);
            }
          };
          it.addMethod(_initMethodName, _function);
          String _cacheFieldName = MethodMemoizer.this.cacheFieldName();
          final Procedure1<MutableFieldDeclaration> _function_1 = new Procedure1<MutableFieldDeclaration>() {
            public void apply(final MutableFieldDeclaration it) {
              boolean _isStatic = MethodMemoizer.this.method.isStatic();
              it.setStatic(_isStatic);
              TypeReference _cacheFieldType = MethodMemoizer.this.cacheFieldType();
              it.setType(_cacheFieldType);
              final CompilationStrategy _function = new CompilationStrategy() {
                public CharSequence compile(final CompilationContext it) {
                  CharSequence _cacheFieldInit = MethodMemoizer.this.cacheFieldInit(it);
                  return _cacheFieldInit;
                }
              };
              it.setInitializer(_function);
            }
          };
          it.addField(_cacheFieldName, _function_1);
        }
      };
      ObjectExtensions.<MutableTypeDeclaration>operator_doubleArrow(_declaringType, _function);
      final Procedure1<MutableMethodDeclaration> _function_1 = new Procedure1<MutableMethodDeclaration>() {
        public void apply(final MutableMethodDeclaration it) {
          final CompilationStrategy _function = new CompilationStrategy() {
            public CharSequence compile(final CompilationContext it) {
              CharSequence _cacheCall = MethodMemoizer.this.cacheCall(it);
              return _cacheCall;
            }
          };
          it.setBody(_function);
          TypeReference _wrappedReturnType = MethodMemoizer.this.wrappedReturnType();
          it.setReturnType(_wrappedReturnType);
        }
      };
      MutableMethodDeclaration _doubleArrow = ObjectExtensions.<MutableMethodDeclaration>operator_doubleArrow(
        this.method, _function_1);
      _xblockexpression = (_doubleArrow);
    }
    return _xblockexpression;
  }
  
  protected final TypeReference wrappedReturnType() {
    TypeReference _returnType = this.method.getReturnType();
    TypeReference _wrapperIfPrimitive = _returnType.getWrapperIfPrimitive();
    return _wrapperIfPrimitive;
  }
  
  protected final String initMethodName() {
    String _simpleName = this.method.getSimpleName();
    String _plus = (_simpleName + "_init");
    return _plus;
  }
  
  protected final String cacheFieldName() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("cache");
    _builder.append(this.index, "");
    _builder.append("_");
    String _simpleName = this.method.getSimpleName();
    _builder.append(_simpleName, "");
    return _builder.toString();
  }
  
  protected abstract CharSequence cacheCall(final CompilationContext context);
  
  protected abstract TypeReference cacheFieldType();
  
  protected abstract CharSequence cacheFieldInit(final CompilationContext context);
}
