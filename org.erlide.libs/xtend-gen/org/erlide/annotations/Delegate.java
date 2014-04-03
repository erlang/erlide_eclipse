/**
 * https://atlas.assembla.com/code/vmat/subversion/nodes/109/reves-ann/trunk/src/main/java/net/virtualmat/reves/Delegate.xtend
 */
package org.erlide.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;
import org.eclipse.xtend.lib.macro.Active;
import org.erlide.annotations.DelegateParticipant;

@Target(ElementType.TYPE)
@Active(DelegateParticipant.class)
public @interface Delegate {
  public Class<?> to();
  public String field() default "delegate";
}
