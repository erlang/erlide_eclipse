package org.erlide.annotations;

import java.util.List;
import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.TransformationParticipant;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableParameterDeclaration;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.annotations.MultipleParameterMethodMemoizer;
import org.erlide.annotations.ParamterlessMethodMemoizer;
import org.erlide.annotations.SingleParameterMethodMemoizer;

@SuppressWarnings("all")
public class MemoizeProcessor implements TransformationParticipant<MutableMethodDeclaration> {
  public void doTransform(final List<? extends MutableMethodDeclaration> methods, @Extension final TransformationContext context) {
    final Procedure1<MutableMethodDeclaration> _function = new Procedure1<MutableMethodDeclaration>() {
      public void apply(final MutableMethodDeclaration it) {
        Iterable<? extends MutableParameterDeclaration> _parameters = it.getParameters();
        int _size = IterableExtensions.size(_parameters);
        switch (_size) {
          case 0:
            int _indexOf = methods.indexOf(it);
            ParamterlessMethodMemoizer _paramterlessMethodMemoizer = new ParamterlessMethodMemoizer(it, context, _indexOf);
            _paramterlessMethodMemoizer.generate();
            break;
          case 1:
            int _indexOf_1 = methods.indexOf(it);
            SingleParameterMethodMemoizer _singleParameterMethodMemoizer = new SingleParameterMethodMemoizer(it, context, _indexOf_1);
            _singleParameterMethodMemoizer.generate();
            break;
          default:
            int _indexOf_2 = methods.indexOf(it);
            MultipleParameterMethodMemoizer _multipleParameterMethodMemoizer = new MultipleParameterMethodMemoizer(it, context, _indexOf_2);
            _multipleParameterMethodMemoizer.generate();
            break;
        }
      }
    };
    IterableExtensions.forEach(methods, _function);
  }
}
