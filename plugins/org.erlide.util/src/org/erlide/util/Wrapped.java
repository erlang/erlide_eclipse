package org.erlide.util;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.eclipse.xtend.lib.macro.Active;

@Target(ElementType.TYPE)
@Retention(RetentionPolicy.SOURCE)
@Active(WrappedProcessor.class)
public @interface Wrapped {
    public boolean cached() default false;

    public boolean synch() default false;
}
