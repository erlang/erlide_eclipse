package org.erlide.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;
import org.eclipse.xtend.lib.macro.Active;
import org.erlide.annotations.WitherParticipant;

@Target(ElementType.TYPE)
@Active(WitherParticipant.class)
public @interface Wither {
}
