package org.erlide.ui.navigator;

import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.navigator.IDescriptionProvider;
import org.erlide.core.erlang.IErlElement;

public class ErlangFileLabelProvider extends LabelProvider implements
		IDescriptionProvider {

	@SuppressWarnings("unused")
	private final NavigatorProblemsDecorator fProblemDecorator;

	public ErlangFileLabelProvider() {
		super();
		fProblemDecorator = new NavigatorProblemsDecorator();
	}

	@Override
	public Image getImage(final Object element) {
		if (element instanceof IErlElement) {
			return PlatformUI.getWorkbench().getSharedImages().getImage(
					ISharedImages.IMG_OBJS_INFO_TSK);

		}
		return null;
	}

	@Override
	public String getText(final Object element) {
		if (element instanceof IErlElement) {
			final IErlElement data = (IErlElement) element;
			return data.toString();// + "= " + data.getValue();
		}
		return null;
	}

	public String getDescription(final Object anElement) {
		if (anElement instanceof IErlElement) {
			final IErlElement data = (IErlElement) anElement;
			return "Property: " + data.toString(); //$NON-NLS-1$
		}
		return null;
	}

	@Override
	public void addListener(final ILabelProviderListener listener) {
		// TODO Auto-generated method stub

	}

	@Override
	public void dispose() {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean isLabelProperty(final Object element, final String property) {
		// TODO Auto-generated method stub
		return true;
	}

	@Override
	public void removeListener(final ILabelProviderListener listener) {
		// TODO Auto-generated method stub

	}

}
