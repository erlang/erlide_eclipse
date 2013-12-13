package org.erlide.annotations;

import java.util.ArrayList;
import org.eclipse.xtend.lib.macro.AbstractFieldProcessor;
import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.MutableFieldDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableTypeDeclaration;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend.lib.macro.declaration.Visibility;
import org.eclipse.xtend.lib.macro.expression.Expression;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.StringExtensions;

@SuppressWarnings("all")
public class ListenerProcessor extends AbstractFieldProcessor {
  public void doTransform(final MutableFieldDeclaration field, @Extension final TransformationContext context) {
    TypeReference _type = field.getType();
    boolean _isPrimitive = _type.isPrimitive();
    if (_isPrimitive) {
      context.addError(field, "Primitives can\'t be used as event.");
    }
    Expression _initializer = field.getInitializer();
    boolean _tripleNotEquals = (_initializer != null);
    if (_tripleNotEquals) {
      context.addError(field, "Listener event dont\'t need an initializer");
    }
    final TypeReference eventType = field.getType();
    TypeReference _newTypeReference = context.newTypeReference(Procedure1.class, eventType);
    final TypeReference eventListType = context.getList(_newTypeReference);
    field.setType(eventListType);
    String _simpleName = field.getSimpleName();
    String _plus = (_simpleName + "Listener");
    field.setSimpleName(_plus);
    final TypeReference lamdaType = context.newTypeReference(Procedure1.class, eventType);
    TypeReference _list = context.getList(lamdaType);
    final TypeReference initFieldType = context.newTypeReference(Function0.class, _list);
    final TypeReference initListType = context.newTypeReference(ArrayList.class, lamdaType);
    final CompilationStrategy _function = new CompilationStrategy() {
      public CharSequence compile(final CompilationStrategy.CompilationContext it) {
        StringConcatenation _builder = new StringConcatenation();
        _builder.append("new ");
        String _javaCode = it.toJavaCode(initFieldType);
        _builder.append(_javaCode, "");
        _builder.append("() {");
        _builder.newLineIfNotEmpty();
        _builder.append("                ");
        _builder.append("public ");
        String _javaCode_1 = it.toJavaCode(eventListType);
        _builder.append(_javaCode_1, "                ");
        _builder.append(" apply() {");
        _builder.newLineIfNotEmpty();
        _builder.append("                ");
        String _javaCode_2 = it.toJavaCode(eventListType);
        _builder.append(_javaCode_2, "                ");
        _builder.append(" _eventList = new ");
        String _javaCode_3 = it.toJavaCode(initListType);
        _builder.append(_javaCode_3, "                ");
        _builder.append("();");
        _builder.newLineIfNotEmpty();
        _builder.append("                    ");
        _builder.append("return _eventList;");
        _builder.newLine();
        _builder.append("                ");
        _builder.append("}");
        _builder.newLine();
        _builder.append("            ");
        _builder.append("}.apply();");
        return _builder;
      }
    };
    field.setInitializer(_function);
    MutableTypeDeclaration _declaringType = field.getDeclaringType();
    String _simpleName_1 = field.getSimpleName();
    String _firstUpper = StringExtensions.toFirstUpper(_simpleName_1);
    String _plus_1 = ("add" + _firstUpper);
    final Procedure1<MutableMethodDeclaration> _function_1 = new Procedure1<MutableMethodDeclaration>() {
      public void apply(final MutableMethodDeclaration it) {
        it.setVisibility(Visibility.PUBLIC);
        it.addParameter("listener", lamdaType);
        final CompilationStrategy _function = new CompilationStrategy() {
          public CharSequence compile(final CompilationStrategy.CompilationContext it) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("this.");
            String _simpleName = field.getSimpleName();
            _builder.append(_simpleName, "");
            _builder.append(".add(listener);");
            return _builder;
          }
        };
        it.setBody(_function);
      }
    };
    _declaringType.addMethod(_plus_1, _function_1);
    MutableTypeDeclaration _declaringType_1 = field.getDeclaringType();
    String _simpleName_2 = field.getSimpleName();
    String _firstUpper_1 = StringExtensions.toFirstUpper(_simpleName_2);
    String _plus_2 = ("notifyAll" + _firstUpper_1);
    final Procedure1<MutableMethodDeclaration> _function_2 = new Procedure1<MutableMethodDeclaration>() {
      public void apply(final MutableMethodDeclaration it) {
        it.setVisibility(Visibility.PUBLIC);
        it.addParameter("event", eventType);
        final CompilationStrategy _function = new CompilationStrategy() {
          public CharSequence compile(final CompilationStrategy.CompilationContext it) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("for (");
            String _javaCode = it.toJavaCode(lamdaType);
            _builder.append(_javaCode, "");
            _builder.append(" listener : ");
            String _simpleName = field.getSimpleName();
            _builder.append(_simpleName, "");
            _builder.append(") {");
            _builder.newLineIfNotEmpty();
            _builder.append("                    ");
            _builder.append("listener.apply(event);");
            _builder.newLine();
            _builder.append("                    ");
            _builder.append("}");
            return _builder;
          }
        };
        it.setBody(_function);
      }
    };
    _declaringType_1.addMethod(_plus_2, _function_2);
  }
}
