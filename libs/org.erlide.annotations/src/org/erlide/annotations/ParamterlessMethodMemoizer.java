package org.erlide.annotations;

import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Extension;

@SuppressWarnings("all")
public class ParamterlessMethodMemoizer extends MethodMemoizer {
    public ParamterlessMethodMemoizer(final MutableMethodDeclaration method,
            final TransformationContext context, final int index) {
        super(method, context, index);
    }

    @Override
    protected CharSequence cacheCall(
            @Extension final CompilationStrategy.CompilationContext context) {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("if (");
        final String _cacheFieldName = cacheFieldName();
        _builder.append(_cacheFieldName);
        _builder.append(" == null) {");
        _builder.newLineIfNotEmpty();
        _builder.append("  ");
        _builder.append("synchronized(");
        final CharSequence _lock = lock();
        _builder.append(_lock, "  ");
        _builder.append(") {");
        _builder.newLineIfNotEmpty();
        _builder.append("    ");
        _builder.append("if (");
        final String _cacheFieldName_1 = cacheFieldName();
        _builder.append(_cacheFieldName_1, "    ");
        _builder.append(" == null) {");
        _builder.newLineIfNotEmpty();
        _builder.append("      ");
        final String _cacheFieldName_2 = cacheFieldName();
        _builder.append(_cacheFieldName_2, "      ");
        _builder.append(" = ");
        final String _initMethodName = initMethodName();
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
        final String _cacheFieldName_3 = cacheFieldName();
        _builder.append(_cacheFieldName_3);
        _builder.append(";");
        _builder.newLineIfNotEmpty();
        return _builder;
    }

    @Override
    protected TypeReference cacheFieldType() {
        return wrappedReturnType();
    }

    @Override
    protected CharSequence cacheFieldInit(
            final CompilationStrategy.CompilationContext context) {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("null");
        return _builder;
    }

    public CharSequence lock() {
        CharSequence _xifexpression = null;
        final boolean _isStatic = method.isStatic();
        if (_isStatic) {
            final StringConcatenation _builder = new StringConcatenation();
            final String _simpleName = method.getDeclaringType().getSimpleName();
            _builder.append(_simpleName);
            _builder.append(".class");
            _xifexpression = _builder;
        } else {
            _xifexpression = "this";
        }
        return _xifexpression;
    }
}
