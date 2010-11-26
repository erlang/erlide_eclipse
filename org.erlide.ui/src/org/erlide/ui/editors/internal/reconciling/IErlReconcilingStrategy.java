package org.erlide.ui.editors.internal.reconciling;

import org.eclipse.jface.text.reconciler.IReconcilingStrategy;

public interface IErlReconcilingStrategy extends IReconcilingStrategy {
    void uninstall();

    void chunkReconciled();

    void reconcile(ErlDirtyRegion r);
}
