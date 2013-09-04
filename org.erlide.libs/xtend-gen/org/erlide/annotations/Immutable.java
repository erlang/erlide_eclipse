package org.erlide.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Target;
import org.eclipse.xtend.lib.macro.Active;
import org.erlide.annotations.ImmutableProcessor;

@Active(ImmutableProcessor.class)
@Documented
@Target(ElementType.TYPE)
public @interface Immutable {
}
