package org.erlide.ui.editors.erl;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.reconciler.IReconcilingStrategy;
import org.eclipse.jface.text.reconciler.MonoReconciler;

public class ErlReconciler extends MonoReconciler {

	public ErlReconciler(final IReconcilingStrategy strategy,
			final boolean isIncremental) {
		super(strategy, isIncremental);
	}

	@Override
	protected void initialProcess() {
		super.initialProcess();
		startReconciling();
	}

	@Override
	public void uninstall() {
		final ErlReconcilerStrategy s = (ErlReconcilerStrategy) getReconcilingStrategy(IDocument.DEFAULT_CONTENT_TYPE);
		s.uninstall();
	}

	@Override
	public void forceReconciling() {
		super.forceReconciling();
	}

}
