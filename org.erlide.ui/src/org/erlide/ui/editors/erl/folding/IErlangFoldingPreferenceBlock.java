/**
 * 
 */
package org.erlide.ui.editors.erl.folding;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * @author jakob
 * 
 */
public interface IErlangFoldingPreferenceBlock {

    /**
     * Creates the control that will be displayed on the Erlang &gt; Editor &gt;
     * Folding preference page.
     * 
     * @param parent
     *            the parent composite to which to add the preferences control
     * @return the control that was added to <code>parent</code>
     */
    Control createControl(Composite parent);

    /**
     * Called after creating the control. Implementations should load the
     * preferences values and update the controls accordingly.
     */
    void initialize();

    /**
     * Called when the <code>OK</code> button is pressed on the preference page.
     * Implementations should commit the configured preference settings into
     * their form of preference storage.
     */
    void performOk();

    /**
     * Called when the <code>Defaults</code> button is pressed on the preference
     * page. Implementation should reset any preference settings to their
     * default values and adjust the controls accordingly.
     */
    void performDefaults();

    /**
     * Called when the preference page is being disposed. Implementations should
     * free any resources they are holding on to.
     */
    void dispose();

}
