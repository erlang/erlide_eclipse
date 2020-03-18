/**
 * Copyright (c) 2013 itemis AG (http://www.itemis.eu) and others. All rights reserved.
 * This program and the accompanying materials are made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 */
package org.erlide.annotations;

import org.eclipse.xtend.lib.macro.AbstractFieldProcessor;
import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.MutableFieldDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableTypeDeclaration;
import org.eclipse.xtend.lib.macro.declaration.Visibility;
import org.eclipse.xtend.lib.macro.expression.Expression;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.StringExtensions;

@SuppressWarnings("all")
public class LazyProcessor extends AbstractFieldProcessor {
    @Override
    public void doTransform(final MutableFieldDeclaration field,
            @Extension final TransformationContext context) {
        final boolean _isPrimitive = field.getType().isPrimitive();
        if (_isPrimitive) {
            context.addError(field, "Fields with primitives are not supported by @Lazy");
        }
        final Expression _initializer = field.getInitializer();
        final boolean _tripleEquals = _initializer == null;
        if (_tripleEquals) {
            context.addError(field, "A lazy field must have an initializer.");
        }
        final MutableTypeDeclaration _declaringType = field.getDeclaringType();
        final String _simpleName = field.getSimpleName();
        final String _plus = "_init" + _simpleName;
        final Procedure1<MutableMethodDeclaration> _function = (
                final MutableMethodDeclaration it) -> {
            it.setVisibility(Visibility.PRIVATE);
            it.setReturnType(field.getType());
            it.setBody(field.getInitializer());
        };
        _declaringType.addMethod(_plus, _function);
        final MutableTypeDeclaration _declaringType_1 = field.getDeclaringType();
        final String _firstUpper = StringExtensions.toFirstUpper(field.getSimpleName());
        final String _plus_1 = "get" + _firstUpper;
        final Procedure1<MutableMethodDeclaration> _function_1 = (
                final MutableMethodDeclaration it) -> {
            it.setReturnType(field.getType());
            final CompilationStrategy _function_2 = (
                    final CompilationStrategy.CompilationContext it_1) -> {
                final StringConcatenation _builder = new StringConcatenation();
                _builder.append("if (");
                final String _simpleName_1 = field.getSimpleName();
                _builder.append(_simpleName_1);
                _builder.append("==null)");
                _builder.newLineIfNotEmpty();
                _builder.append("  ");
                final String _simpleName_2 = field.getSimpleName();
                _builder.append(_simpleName_2, "  ");
                _builder.append(" = _init");
                final String _simpleName_3 = field.getSimpleName();
                _builder.append(_simpleName_3, "  ");
                _builder.append("();");
                _builder.newLineIfNotEmpty();
                _builder.append("return ");
                final String _simpleName_4 = field.getSimpleName();
                _builder.append(_simpleName_4);
                _builder.append(";");
                _builder.newLineIfNotEmpty();
                return _builder;
            };
            it.setBody(_function_2);
        };
        _declaringType_1.addMethod(_plus_1, _function_1);
    }
}
