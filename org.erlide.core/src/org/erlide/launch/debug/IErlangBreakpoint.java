package org.erlide.launch.debug;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.model.IBreakpoint;

public interface IErlangBreakpoint extends IBreakpoint {
    /**
     * Returns whether this breakpoint supports a conditional expression.
     * Conditional breakpoints only suspend when their associated condition
     * evaluates to <code>true</code>.
     * 
     * @return whether this breakpoint supports a condition
     */
    public boolean supportsCondition();

    /**
     * Returns the conditional expression associated with this breakpoint, or
     * <code>null</code> if this breakpoint does not have a condition.
     * 
     * @return this breakpoint's conditional expression, or <code>null</code>
     * @exception CoreException
     *                if unable to access the property on this breakpoint's
     *                underlying marker
     */
    public String getCondition() throws CoreException;

    /**
     * Sets the condition associated with this breakpoint. When the condition is
     * enabled, this breakpoint will only suspend execution when the given
     * condition evaluates to <code>true</code>. Setting the condition to
     * <code>null</code> or an empty string removes the condition.
     * <p>
     * If this breakpoint does not support conditions, setting the condition has
     * no effect.
     * </p>
     * 
     * @param condition
     *            conditional expression
     * @exception CoreException
     *                if unable to set the property on this breakpoint's
     *                underlying marker
     */
    public void setCondition(String condition) throws CoreException;

    /**
     * Returns whether the condition on this breakpoint is enabled.
     * 
     * @return whether this breakpoint's condition is enabled
     * @exception CoreException
     *                if unable to access the property on this breakpoint's
     *                underlying marker
     */
    public boolean isConditionEnabled() throws CoreException;

    /**
     * Sets the enabled state of this breakpoint's condition to the given state.
     * When enabled, this breakpoint will only suspend when its condition
     * evaluates to true. When disabled, this breakpoint will suspend as it
     * would with no condition defined.
     * 
     * @exception CoreException
     *                if unable to set the property on this breakpoint's
     *                underlying marker
     */
    public void setConditionEnabled(boolean enabled) throws CoreException;

    /**
     * Returns whether the breakpoint suspends when the value of the condition
     * is <code>true</code> or when the value of the condition changes.
     * 
     * @return <code>true</code> if this breakpoint suspends when the value of
     *         the condition is <code>true</code>, <code>false</code> if this
     *         breakpoint suspends when the value of the condition changes.
     * @exception CoreException
     *                if unable to access the property on this breakpoint's
     *                underlying marker
     * @since 2.1
     */
    // public boolean isConditionSuspendOnTrue() throws CoreException;
    /**
     * Set the suspend state of this breakpoint's condition. If the value is
     * <code>true</code>, the breakpoint will stop when the value of the
     * condition is <code>true</code>. If the value is <code>false</code>, the
     * breakpoint will stop when the value of the condition changes.
     * 
     * @exception CoreException
     *                if unable to access the property on this breakpoint's
     *                underlying marker
     * @since 2.1
     */
    // public void setConditionSuspendOnTrue(boolean suspendOnTrue)
    // throws CoreException;
    /**
     * Set the hit count, the number of times it can hit and continue
     */
    public void setHitCount(int hitCount);

    public int getHitCount();

    public static final int BREAK_ACTION_BREAK = 0;
    public static final int BREAK_ACTION_TRACE_AND_CONTINUE = 1;

    public void setBreakAction(int traceAction);

    public int getBreakAction();

    public String getMessage();
    // public String getTypeName();
}
