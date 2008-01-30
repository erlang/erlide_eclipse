package org.erlide.ui.editors.erl;

import org.eclipse.jface.text.reconciler.IReconcilingStrategy;
import org.eclipse.jface.text.reconciler.MonoReconciler;

public class ErlReconciler extends MonoReconciler {

	public ErlReconciler(IReconcilingStrategy strategy, boolean isIncremental) {
		super(strategy, isIncremental);
		// TODO Auto-generated constructor stub
	}

	// @Override
	// protected synchronized void startReconciling() {
	// super.startReconciling();
	// forceReconciling();
	// }

	@Override
	protected void initialProcess() {
		super.initialProcess();
		startReconciling();
	}

}
