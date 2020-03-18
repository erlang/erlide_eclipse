package org.erlide.annotations;

import java.util.function.Consumer;

import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtend.lib.macro.AbstractClassProcessor;
import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.AnnotationReference;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.MutableClassDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableFieldDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.StringExtensions;

import com.google.common.base.Objects;

@SuppressWarnings("all")
public class WitherParticipant extends AbstractClassProcessor {
    @Override
    public void doTransform(final MutableClassDeclaration annotatedClass,
            @Extension final TransformationContext context) {
        final Function1<AnnotationReference, Boolean> _function = (
                final AnnotationReference it) -> {
            final String _canonicalName = Data.class.getCanonicalName();
            final String _qualifiedName = it.getAnnotationTypeDeclaration()
                    .getQualifiedName();
            return Boolean.valueOf(Objects.equal(_canonicalName, _qualifiedName));
        };
        final boolean dataClass = IterableExtensions
                .exists(annotatedClass.getAnnotations(), _function);
        if (!dataClass) {
            context.addError(annotatedClass,
                    "Class needs to be annotated with @Data to be able to use @Wither");
            return;
        }
        final Function1<MutableFieldDeclaration, Boolean> _function_1 = (
                final MutableFieldDeclaration it) -> {
            final boolean _isStatic = it.isStatic();
            return Boolean.valueOf(!_isStatic);
        };
        final Iterable<? extends MutableFieldDeclaration> fields = IterableExtensions
                .filter(annotatedClass.getDeclaredFields(), _function_1);
        final Consumer<MutableFieldDeclaration> _function_2 = (
                final MutableFieldDeclaration field) -> {
            final String properName = field.getSimpleName().substring(1);
            final String capitalizedName = StringExtensions.toFirstUpper(properName);
            final String methodName = "with" + capitalizedName;
            final Function1<MutableMethodDeclaration, Boolean> _function_3 = (
                    final MutableMethodDeclaration it) -> {
                final String _simpleName = it.getSimpleName();
                return Boolean.valueOf(Objects.equal(_simpleName, methodName));
            };
            final boolean _exists = IterableExtensions
                    .exists(annotatedClass.getDeclaredMethods(), _function_3);
            final boolean _not = !_exists;
            if (_not) {
                final Procedure1<MutableMethodDeclaration> _function_4 = (
                        final MutableMethodDeclaration it) -> {
                    it.addParameter(properName, field.getType());
                    it.setReturnType(context.newTypeReference(annotatedClass));
                    final CompilationStrategy _function_5 = (
                            final CompilationStrategy.CompilationContext it_1) -> {
                        final StringConcatenation _builder = new StringConcatenation();
                        _builder.append("return new ");
                        final String _simpleName = annotatedClass.getSimpleName();
                        _builder.append(_simpleName);
                        _builder.append("(");
                        _builder.newLineIfNotEmpty();
                        {
                            boolean _hasElements = false;
                            for (final MutableFieldDeclaration f : fields) {
                                if (!_hasElements) {
                                    _hasElements = true;
                                } else {
                                    _builder.appendImmediate(",", "");
                                }
                                {
                                    final boolean _equals = Objects.equal(f, field);
                                    if (_equals) {
                                        _builder.append(properName);
                                        _builder.newLineIfNotEmpty();
                                    } else {
                                        final String _simpleName_1 = f.getSimpleName();
                                        _builder.append(_simpleName_1);
                                        _builder.newLineIfNotEmpty();
                                    }
                                }
                            }
                        }
                        _builder.append(");");
                        _builder.newLine();
                        return _builder;
                    };
                    it.setBody(_function_5);
                };
                annotatedClass.addMethod(methodName, _function_4);
            }
        };
        fields.forEach(_function_2);
    }
}
