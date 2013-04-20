package org.erlide.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;
import org.eclipse.xtend.lib.macro.Active;
import org.erlide.annotations.MemoizeProcessor;

@Target(ElementType.METHOD)
@Active(MemoizeProcessor.class)
public @interface Memoize {
  public long cacheDuration() default Long.MAX_VALUE;
  public long maxSize() default Long.MAX_VALUE;
}
