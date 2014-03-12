package org.erlide.ui.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

public class XtendSWTLib {

    public static Shell newShell(final Display disp, final Procedure1<Shell> init) {
        final Shell result = new Shell(disp);
        init.apply(result);
        return result;
    }

    public static <T> T newControl(final Composite parent, final Class<T> clazz,
            final int style, final Procedure1<T> init) throws IllegalArgumentException,
            InstantiationException, IllegalAccessException, InvocationTargetException,
            SecurityException, NoSuchMethodException {
        final Constructor<T> cstr = clazz.getConstructor(Composite.class, int.class);
        final T c = cstr.newInstance(parent, style);
        init.apply(c);
        return c;
    }

    public static GridData newGridData(final Procedure1<GridData> init) {
        final GridData gd = new GridData();
        init.apply(gd);
        return gd;
    }

    public static MessageBox newMessageBox(final Shell shell, final int style,
            final Procedure1<MessageBox> init) {
        final MessageBox box = new MessageBox(shell);
        init.apply(box);
        return box;
    }

}
