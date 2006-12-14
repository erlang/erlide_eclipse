/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.views;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialogWithToggle;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextPresentation;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.util.Assert;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTError;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.IAbstractTextEditorHelpContextIds;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.editors.erl.ErlTextHover;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.util.HTMLPrinter;
import org.erlide.ui.editors.util.HTMLTextPresenter;
import org.osgi.framework.Bundle;

/**
 * View which shows Javadoc for a given Java element.
 * 
 * FIXME: As of 3.0 selectAll() and getSelection() is not working see
 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=63022
 * 
 * @since 3.0
 */
public class EdocView extends AbstractInfoView {

	/**
	 * Preference key for the preference whether to show a dialog when the SWT
	 * Browser widget is not available.
	 * 
	 * @since 3.0
	 */
	private static final String DO_NOT_WARN_PREFERENCE_KEY = "JavadocView.error.doNotWarn"; //$NON-NLS-1$

	// see https://bugs.eclipse.org/bugs/show_bug.cgi?id=73558
	private static final boolean WARNING_DIALOG_ENABLED = false;

	// /** Flags used to render a label in the text widget. */
	// private static final long LABEL_FLAGS=
	// JavaElementLabels.ALL_FULLY_QUALIFIED
	// | JavaElementLabels.M_PRE_RETURNTYPE |
	// JavaElementLabels.M_PARAMETER_TYPES |
	// JavaElementLabels.M_PARAMETER_NAMES | JavaElementLabels.M_EXCEPTIONS
	// | JavaElementLabels.F_PRE_TYPE_SIGNATURE |
	// JavaElementLabels.T_TYPE_PARAMETERS;

	/** The HTML widget. */
	private Browser fBrowser;

	/** The text widget. */
	private StyledText fText;

	/** The information presenter. */
	private DefaultInformationControl.IInformationPresenterExtension fPresenter;

	/** The text presentation. */
	private final TextPresentation fPresentation = new TextPresentation();

	/** The select all action */
	private SelectAllAction fSelectAllAction;

	/** The URL of the style sheet (css) */
	private URL fStyleSheetURL;

	/** The Browser widget */
	private boolean fIsUsingBrowserWidget;

	/**
	 * The Javadoc view's select all action.
	 */
	private class SelectAllAction extends Action {

		/** The control. */
		private Control fControl;

		/** The selection provider. */
		private SelectionProvider fSelectionProvider;

		/**
		 * Creates the action.
		 * 
		 * @param control
		 *            the widget
		 * @param selectionProvider
		 *            the selection provider
		 */
		public SelectAllAction(Control control,
				SelectionProvider selectionProvider) {
			super("selectAll"); //$NON-NLS-1$

			Assert.isNotNull(control);
			Assert.isNotNull(selectionProvider);
			fControl = control;
			fSelectionProvider = selectionProvider;

			// FIXME: see https://bugs.eclipse.org/bugs/show_bug.cgi?id=63022
			setEnabled(!fIsUsingBrowserWidget);

			setText("Select All");
			setToolTipText("Select All");
			setDescription("Select All");

			PlatformUI.getWorkbench().getHelpSystem().setHelp(this,
					IAbstractTextEditorHelpContextIds.SELECT_ALL_ACTION);
		}

		/**
		 * Selects all in the view.
		 */
		@Override
		public void run() {
			if (fControl instanceof StyledText) {
				((StyledText) fControl).selectAll();
			} else {
				// FIXME: see
				// https://bugs.eclipse.org/bugs/show_bug.cgi?id=63022
				// ((Browser)fControl).selectAll();
				if (fSelectionProvider != null) {
					fSelectionProvider.fireSelectionChanged();
				}
			}
		}
	}

	/**
	 * The Javadoc view's selection provider.
	 */
	private static class SelectionProvider implements ISelectionProvider {

		/** The selection changed listeners. */
		private final List<ISelectionChangedListener> fListeners = new ArrayList<ISelectionChangedListener>(
				0);

		/** The widget. */
		private Control fControl;

		/**
		 * Creates a new selection provider.
		 * 
		 * @param control
		 *            the widget
		 */
		public SelectionProvider(Control control) {
			Assert.isNotNull(control);
			fControl = control;
			if (fControl instanceof StyledText) {
				((StyledText) fControl)
						.addSelectionListener(new SelectionAdapter() {

							@Override
							public void widgetSelected(SelectionEvent e) {
								fireSelectionChanged();
							}
						});
			} else {
				// FIXME: see
				// https://bugs.eclipse.org/bugs/show_bug.cgi?id=63022
				// ((Browser)fControl).addSelectionListener(new
				// SelectionAdapter() {
				// public void widgetSelected(SelectionEvent e) {
				// fireSelectionChanged();
				// }
				// });
			}
		}

		/**
		 * Sends a selection changed event to all listeners.
		 */
		public void fireSelectionChanged() {
			final ISelection selection = getSelection();
			final SelectionChangedEvent event = new SelectionChangedEvent(this,
					selection);
			final Object[] selectionChangedListeners = fListeners.toArray();
			for (Object element : selectionChangedListeners) {
				((ISelectionChangedListener) element).selectionChanged(event);
			}
		}

		/*
		 * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
		 */
		public void addSelectionChangedListener(
				ISelectionChangedListener listener) {
			fListeners.add(listener);
		}

		/*
		 * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
		 */
		public ISelection getSelection() {
			if (fControl instanceof StyledText) {
				final IDocument document = new Document(((StyledText) fControl)
						.getSelectionText());
				return new TextSelection(document, 0, document.getLength());
			} else {
				// FIXME: see
				// https://bugs.eclipse.org/bugs/show_bug.cgi?id=63022
				return StructuredSelection.EMPTY;
			}
		}

		/*
		 * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
		 */
		public void removeSelectionChangedListener(
				ISelectionChangedListener listener) {
			fListeners.remove(listener);
		}

		/*
		 * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
		 */
		public void setSelection(ISelection selection) {
			// not supported
		}
	}

	/*
	 * @see AbstractInfoView#internalCreatePartControl(Composite)
	 */
	@Override
	protected void internalCreatePartControl(Composite parent) {
		try {
			fBrowser = new Browser(parent, SWT.NONE);
			fIsUsingBrowserWidget = true;
		} catch (final SWTError er) {

			/*
			 * The Browser widget throws an SWTError if it fails to instantiate
			 * properly. Application code should catch this SWTError and disable
			 * any feature requiring the Browser widget. Platform requirements
			 * for the SWT Browser widget are available from the SWT FAQ web
			 * site.
			 */

			final IPreferenceStore store = ErlideUIPlugin.getDefault()
					.getPreferenceStore();
			boolean doNotWarn = store.getBoolean(DO_NOT_WARN_PREFERENCE_KEY);
			if (WARNING_DIALOG_ENABLED && !doNotWarn) {
				final String title = "Error";
				final String message = "Error no browser found";
				final String toggleMessage = "Don't show this again";
				final MessageDialogWithToggle dialog = MessageDialogWithToggle
						.openError(parent.getShell(), title, message,
								toggleMessage, false, null, null);
				if (dialog.getReturnCode() == Window.OK) {
					store.setValue(DO_NOT_WARN_PREFERENCE_KEY, dialog
							.getToggleState());
				}
			}

			fIsUsingBrowserWidget = false;
		}

		if (!fIsUsingBrowserWidget) {
			fText = new StyledText(parent, SWT.V_SCROLL | SWT.H_SCROLL);
			fText.setEditable(false);
			fPresenter = new HTMLTextPresenter(false);

			fText.addControlListener(new ControlAdapter() {

				/*
				 * @see org.eclipse.swt.events.ControlAdapter#controlResized(org.eclipse.swt.events.ControlEvent)
				 */
				@Override
				public void controlResized(ControlEvent e) {
					setInfo(fText.getText());
				}
			});
		}

		initStyleSheetURL();
		getViewSite().setSelectionProvider(new SelectionProvider(getControl()));
	}

	private void initStyleSheetURL() {
		final Bundle bundle = Platform.getBundle(ErlideUIPlugin.PLUGIN_ID);
		fStyleSheetURL = bundle.getEntry("/JavadocViewStyleSheet.css"); //$NON-NLS-1$
		if (fStyleSheetURL == null) {
			return;
		}

		try {
			fStyleSheetURL = FileLocator.toFileURL(fStyleSheetURL);
		} catch (final IOException ex) {
			ErlideUIPlugin.log(ex);
		}
	}

	/*
	 * @see AbstractInfoView#createActions()
	 */
	@Override
	protected void createActions() {
		super.createActions();
		fSelectAllAction = new SelectAllAction(getControl(),
				(SelectionProvider) getSelectionProvider());
	}

	/*
	 * @see org.eclipse.jdt.internal.ui.infoviews.AbstractInfoView#getSelectAllAction()
	 * @since 3.0
	 */
	@Override
	protected IAction getSelectAllAction() {
		// FIXME: see https://bugs.eclipse.org/bugs/show_bug.cgi?id=63022
		if (fIsUsingBrowserWidget) {
			return null;
		}

		return fSelectAllAction;
	}

	/*
	 * @see org.eclipse.jdt.internal.ui.infoviews.AbstractInfoView#getCopyToClipboardAction()
	 * @since 3.0
	 */
	@Override
	protected IAction getCopyToClipboardAction() {
		// FIXME: see https://bugs.eclipse.org/bugs/show_bug.cgi?id=63022
		if (fIsUsingBrowserWidget) {
			return null;
		}

		return super.getCopyToClipboardAction();
	}

	/*
	 * @see AbstractInfoView#setForeground(Color)
	 */
	@Override
	protected void setForeground(Color color) {
		getControl().setForeground(color);
	}

	/*
	 * @see AbstractInfoView#setBackground(Color)
	 */
	@Override
	protected void setBackground(Color color) {
		getControl().setBackground(color);
	}

	/*
	 * @see AbstractInfoView#internalDispose()
	 */
	@Override
	protected void internalDispose() {
		fText = null;
		fBrowser = null;
	}

	/*
	 * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
	 */
	@Override
	public void setFocus() {
		getControl().setFocus();
	}

	// /*
	// * @see AbstractInfoView#computeInput(Object)
	// */
	// protected Object computeInput(Object input) {
	// if (getControl() == null || ! (input instanceof IJavaElement))
	// return null;
	//
	// IJavaElement je= (IJavaElement)input;
	// String javadocHtml;
	//
	// switch (je.getElementType()) {
	// case IJavaElement.COMPILATION_UNIT:
	// try {
	// javadocHtml= getJavadocHtml(((ICompilationUnit)je).getTypes());
	// } catch (JavaModelException ex) {
	// javadocHtml= null;
	// }
	// break;
	// case IJavaElement.CLASS_FILE:
	// try {
	// javadocHtml= getJavadocHtml(new IJavaElement[]
	// {((IClassFile)je).getType()});
	// } catch (JavaModelException ex) {
	// javadocHtml= null;
	// }
	// break;
	// default:
	// javadocHtml= getJavadocHtml(new IJavaElement[] { je });
	// }
	//
	// return javadocHtml;
	// }
	//
	/*
	 * @see AbstractInfoView#setInfo(String)
	 */
	@Override
	protected void setInfo(String info) {
		String javadocHtml = info;

		if (fIsUsingBrowserWidget) {
			if (javadocHtml != null && javadocHtml.length() > 0) {
				final boolean RTL = (getSite().getShell().getStyle() & SWT.RIGHT_TO_LEFT) != 0;
				if (RTL) {
					final StringBuffer buffer = new StringBuffer(javadocHtml);
					HTMLPrinter.insertStyles(buffer,
							new String[] { "direction:rtl" }); //$NON-NLS-1$
					javadocHtml = buffer.toString();
				}
			}
			fBrowser.setText(javadocHtml);
		} else {
			fPresentation.clear();
			final Rectangle size = fText.getClientArea();

			try {
				javadocHtml = fPresenter.updatePresentation(getSite()
						.getShell(), javadocHtml, fPresentation, size.width,
						size.height);
			} catch (final IllegalArgumentException ex) {
				// the javadoc might no longer be valid
				return;
			}
			fText.setText(javadocHtml);
			TextPresentation.applyTextPresentation(fPresentation, fText);
		}
	}

	// /**
	// * Returns the Javadoc in HTML format.
	// *
	// * @param result the Java elements for which to get the Javadoc
	// * @return a string with the Javadoc in HTML format.
	// */
	// private String getJavadocHtml(IJavaElement[] result) {
	// StringBuffer buffer= new StringBuffer();
	// int nResults= result.length;
	//
	// if (nResults == 0)
	// return null;
	//
	// if (nResults > 1) {
	//
	// for (int i= 0; i < result.length; i++) {
	// HTMLPrinter.startBulletList(buffer);
	// IJavaElement curr= result[i];
	// if (curr instanceof IMember)
	// HTMLPrinter.addBullet(buffer, getInfoText((IMember) curr));
	// HTMLPrinter.endBulletList(buffer);
	// }
	//
	// } else {
	//
	// IJavaElement curr= result[0];
	// if (curr instanceof IMember) {
	// IMember member= (IMember) curr;
	// // HTMLPrinter.addSmallHeader(buffer, getInfoText(member));
	// Reader reader;
	// try {
	// reader= JavadocContentAccess.getHTMLContentReader(member, true, true);
	// } catch (JavaModelException ex) {
	// return null;
	// }
	// if (reader != null) {
	// HTMLPrinter.addParagraph(buffer, reader);
	// }
	// }
	// }
	//
	// if (buffer.length() > 0) {
	// HTMLPrinter.insertPageProlog(buffer, 0, fStyleSheetURL);
	// HTMLPrinter.addPageEpilog(buffer);
	// return buffer.toString();
	// }
	//
	// return null;
	// }

	// /**
	// * Gets the label for the given member.
	// *
	// * @param member the Java member
	// * @return a string containing the member's label
	// */
	// private String getInfoText(IMember member) {
	// return JavaElementLabels.getElementLabel(member, LABEL_FLAGS);
	// }

	/*
	 * @see org.eclipse.jdt.internal.ui.infoviews.AbstractInfoView#isIgnoringEqualInput()
	 * @since 3.0
	 */
	@Override
	protected boolean isIgnoringEqualInput() {
		return false;
	}

	// /*
	// * @see AbstractInfoView#findSelectedJavaElement(IWorkbenchPart)
	// */
	// protected IJavaElement findSelectedJavaElement(IWorkbenchPart part,
	// ISelection
	// selection) {
	// IJavaElement element;
	// try {
	// element= super.findSelectedJavaElement(part, selection);
	//
	// if (element == null && part instanceof JavaEditor && selection instanceof
	// ITextSelection) {
	//
	// JavaEditor editor= (JavaEditor)part;
	// ITextSelection textSelection= (ITextSelection)selection;
	//
	// IDocumentProvider documentProvider= editor.getDocumentProvider();
	// if (documentProvider == null)
	// return null;
	//
	// IDocument document=
	// documentProvider.getDocument(editor.getEditorInput());
	// if (document == null)
	// return null;
	//
	// ITypedRegion typedRegion= TextUtilities.getPartition(document,
	// IJavaPartitions.JAVA_PARTITIONING, textSelection.getOffset(), false);
	// if (IJavaPartitions.JAVA_DOC.equals(typedRegion.getType()))
	// return TextSelectionConverter.getElementAtOffset((JavaEditor)part,
	// textSelection);
	// else
	// return null;
	// } else
	// return element;
	// } catch (JavaModelException e) {
	// return null;
	// } catch (BadLocationException e) {
	// return null;
	// }
	// }

	/*
	 * @see AbstractInfoView#getControl()
	 */
	@Override
	protected Control getControl() {
		if (fIsUsingBrowserWidget) {
			return fBrowser;
		} else {
			return fText;
		}
	}

	/*
	 * @see org.eclipse.jdt.internal.ui.infoviews.AbstractInfoView#getHelpContextId()
	 * @since 3.1
	 */
	@Override
	protected String getHelpContextId() {
		return ""; // TODO return IJavaHelpContextIds.JAVADOC_VIEW;
	}

	@Override
	protected String getInfoForSelection(IWorkbenchPart part,
			ISelection selection) {
		if (selection instanceof ITextSelection && part instanceof ErlangEditor) {
			final ITextSelection sel = (ITextSelection) selection;
			final ErlangEditor editor = (ErlangEditor) part;
			return ErlTextHover.getHoverTextForOffset(sel.getOffset(), editor);
		}
		return null;
	}
}
