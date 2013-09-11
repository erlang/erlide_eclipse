package org.erlide.annotations;

import com.google.common.base.Objects;
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
        List<MutableParameterDeclaration> _parameters = it.getParameters();
        int _size = _parameters.size();
        final int _switchValue = _size;
        boolean _matched = false;
        if (!_matched) {
          if (Objects.equal(_switchValue,0)) {
            _matched=true;
            int _indexOf = methods.indexOf(it);
            ParamterlessMethodMemoizer _paramterlessMethodMemoizer = new ParamterlessMethodMemoizer(it, context, _indexOf);
            _paramterlessMethodMemoizer.generate();
          }
        }
        if (!_matched) {
          if (Objects.equal(_switchValue,1)) {
            _matched=true;
            int _indexOf_1 = methods.indexOf(it);
            SingleParameterMethodMemoizer _singleParameterMethodMemoizer = new SingleParameterMethodMemoizer(it, context, _indexOf_1);
            _singleParameterMethodMemoizer.generate();
          }
        }
        if (!_matched) {
          int _indexOf_2 = methods.indexOf(it);
          MultipleParameterMethodMemoizer _multipleParameterMethodMemoizer = new MultipleParameterMethodMemoizer(it, context, _indexOf_2);
          _multipleParameterMethodMemoizer.generate();
        }
      }
    };
    IterableExtensions.forEach(methods, _function);
  }
}
