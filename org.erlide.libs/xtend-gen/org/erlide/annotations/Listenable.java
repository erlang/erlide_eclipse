package org.erlide.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;
import org.eclipse.xtend.lib.macro.Active;
import org.erlide.annotations.ListenerProcessor;

/**
 * Transforms the field to a listenerList. Also two methods will be added:
 * 1. addBallEventListener((fieldType)=>void listener)
 * 2. notifyAllBallEventListener(fieldType event)
 */
@Target(ElementType.FIELD)
@Active(ListenerProcessor.class)
public @interface Listenable {
}
