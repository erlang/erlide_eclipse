package org.erlide.annotations;

import java.util.List;
import java.util.function.Consumer;

import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.TransformationParticipant;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.IterableExtensions;

@SuppressWarnings("all")
public class MemoizeProcessor
        implements TransformationParticipant<MutableMethodDeclaration> {
    @Override
    public void doTransform(final List<? extends MutableMethodDeclaration> methods,
            @Extension final TransformationContext context) {
        final Consumer<MutableMethodDeclaration> _function = (
                final MutableMethodDeclaration it) -> {
            final int _size = IterableExtensions.size(it.getParameters());
            switch (_size) {
            case 0:
                final int _indexOf = methods.indexOf(it);
                new ParamterlessMethodMemoizer(it, context, _indexOf).generate();
                break;
            case 1:
                final int _indexOf_1 = methods.indexOf(it);
                new SingleParameterMethodMemoizer(it, context, _indexOf_1).generate();
                break;
            default:
                final int _indexOf_2 = methods.indexOf(it);
                new MultipleParameterMethodMemoizer(it, context, _indexOf_2).generate();
                break;
            }
        };
        methods.forEach(_function);
    }
}
