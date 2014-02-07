package org.erlide.annotations;

import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableTypeDeclaration;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Extension;
import org.erlide.annotations.MethodMemoizer;

@SuppressWarnings("all")
public class ParamterlessMethodMemoizer extends MethodMemoizer {
  public ParamterlessMethodMemoizer(final MutableMethodDeclaration method, final TransformationContext context, final int index) {
    super(method, context, index);
  }
  
  protected CharSequence cacheCall(@Extension final CompilationStrategy.CompilationContext context) {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("if (");
    String _cacheFieldName = this.cacheFieldName();
    _builder.append(_cacheFieldName, "");
    _builder.append(" == null) {");
    _builder.newLineIfNotEmpty();
    _builder.append("  ");
    _builder.append("synchronized(");
    CharSequence _lock = this.lock();
    _builder.append(_lock, "  ");
    _builder.append(") {");
    _builder.newLineIfNotEmpty();
    _builder.append("    ");
    _builder.append("if (");
    String _cacheFieldName_1 = this.cacheFieldName();
    _builder.append(_cacheFieldName_1, "    ");
    _builder.append(" == null) {");
    _builder.newLineIfNotEmpty();
    _builder.append("      ");
    String _cacheFieldName_2 = this.cacheFieldName();
    _builder.append(_cacheFieldName_2, "      ");
    _builder.append(" = ");
    String _initMethodName = this.initMethodName();
    _builder.append(_initMethodName, "      ");
    _builder.append("();");
    _builder.newLineIfNotEmpty();
    _builder.append("    ");
    _builder.append("}");
    _builder.newLine();
    _builder.append("  ");
    _builder.append("}");
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    _builder.append("return ");
    String _cacheFieldName_3 = this.cacheFieldName();
    _builder.append(_cacheFieldName_3, "");
    _builder.append(";");
    _builder.newLineIfNotEmpty();
    return _builder;
  }
  
  protected TypeReference cacheFieldType() {
    return this.wrappedReturnType();
  }
  
  protected CharSequence cacheFieldInit(final CompilationStrategy.CompilationContext context) {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("null");
    return _builder;
  }
  
  public CharSequence lock() {
    CharSequence _xifexpression = null;
    boolean _isStatic = this.method.isStatic();
    if (_isStatic) {
      StringConcatenation _builder = new StringConcatenation();
      MutableTypeDeclaration _declaringType = this.method.getDeclaringType();
      String _simpleName = _declaringType.getSimpleName();
      _builder.append(_simpleName, "");
      _builder.append(".class");
      _xifexpression = _builder;
    } else {
      _xifexpression = "this";
    }
    return _xifexpression;
  }
}
