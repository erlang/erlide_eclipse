/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl;

import java.net.URL;
import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.Platform;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IVariable;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.internal.text.html.BrowserInformationControl;
import org.eclipse.jface.internal.text.html.BrowserInformationControlInput;
import org.eclipse.jface.internal.text.html.BrowserInput;
import org.eclipse.jface.internal.text.html.HTMLPrinter;
import org.eclipse.jface.internal.text.html.HTMLTextPresenter;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.AbstractReusableInformationControlCreator;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.IInputChangedListener;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.ITextHoverExtension;
import org.eclipse.jface.text.ITextHoverExtension2;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.information.IInformationProviderExtension2;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.editors.text.EditorsUI;
import org.erlide.backend.Backend;
import org.erlide.backend.util.Util;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlScanner;
import org.erlide.core.erlang.ErlToken;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.ErlideUIPluginImages;
import org.erlide.ui.actions.OpenAction;
import org.erlide.ui.editors.util.EditorUtility;
import org.erlide.ui.internal.ErlBrowserInformationControlInput;
import org.erlide.ui.util.ErlModelUtils;
import org.erlide.ui.views.EdocView;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideDoc;
import erlang.OpenResult;

public class ErlTextHover implements ITextHover,
		IInformationProviderExtension2, ITextHoverExtension,
		ITextHoverExtension2 {

	private final IErlModule fModule;
	private static URL fgStyleSheet;
	private IInformationControlCreator fHoverControlCreator;
	private PresenterControlCreator fPresenterControlCreator;
	private final ErlangEditor fEditor;

	public ErlTextHover(final ErlangEditor editor, final IErlModule module) {
		fEditor = editor;
		fModule = module;
		initStyleSheet();
	}

	public IRegion getHoverRegion(final ITextViewer textViewer, final int offset) {
		if (fEditor != null) {
			fEditor.reconcileNow();
		}
		final ErlToken token = fModule.getScanner().getTokenAt(offset);
		if (token == null) {
			return null;
		}
		return new Region(token.getOffset(), token.getLength());
	}

	private void initStyleSheet() {
		final Bundle bundle = Platform.getBundle(ErlideUIPlugin.PLUGIN_ID);
		fgStyleSheet = bundle.getEntry("/edoc.css"); //$NON-NLS-1$
		if (fgStyleSheet != null) {

			try {
				fgStyleSheet = FileLocator.toFileURL(fgStyleSheet);
			} catch (final Exception e) {
			}
		}
	}

	/**
	 * @param textViewer
	 * @param offset
	 * @param length
	 */
	private static String makeDebuggerVariableHover(
			final ITextViewer textViewer, final int offset, final int length) {
		final IAdaptable adaptable = DebugUITools.getDebugContext();
		if (adaptable != null) {
			final IStackFrame frame = (IStackFrame) adaptable
					.getAdapter(IStackFrame.class);
			try {
				if (frame != null && frame.hasVariables()) {
					String varName = "";
					try {
						varName = textViewer.getDocument().get(offset, length);
					} catch (final BadLocationException e) {
					}
					if (varName.length() > 0) {
						final String firstLetter = varName.substring(0, 1);
						if (firstLetter.toUpperCase().equals(firstLetter)) {
							final IVariable[] vars = frame.getVariables();
							for (final IVariable variable : vars) {
								if (variable.getName().equals(varName)) {
									final String value = variable.getValue()
											.getValueString();
									return makeVariablePresentation(varName,
											value);
								}
							}
						}
					}
				}
			} catch (final DebugException e) {
			}
		}
		return "";
	}

	private static String makeVariablePresentation(final String varName,
			final String value) {
		return varName + " = " + value;
	}

	public IInformationControlCreator getInformationPresenterControlCreator() {
		if (fPresenterControlCreator == null) {
			fPresenterControlCreator = new PresenterControlCreator();
		}
		return fPresenterControlCreator;
	}

	public static final class PresenterControlCreator extends
			AbstractReusableInformationControlCreator {

		@SuppressWarnings("restriction")
		@Override
		protected IInformationControl doCreateInformationControl(
				final Shell parent) {
			if (BrowserInformationControl.isAvailable(parent)) {
				try {
					final ToolBarManager tbm = new ToolBarManager(SWT.FLAT);

					final String font = JFaceResources.DIALOG_FONT;
					final BrowserInformationControl iControl = new BrowserInformationControl(
							parent, font, tbm);

					final BackAction backAction = new BackAction(iControl);
					backAction.setEnabled(false);
					tbm.add(backAction);
					final ForwardAction forwardAction = new ForwardAction(
							iControl);
					tbm.add(forwardAction);
					forwardAction.setEnabled(false);

					final ShowInJavadocViewAction showInJavadocViewAction = new ShowInJavadocViewAction(
							iControl);
					tbm.add(showInJavadocViewAction);
					final OpenDeclarationAction openDeclarationAction = new OpenDeclarationAction(
							iControl);
					tbm.add(openDeclarationAction);

					final SimpleSelectionProvider selectionProvider = new SimpleSelectionProvider();
					// OpenExternalBrowserAction openExternalJavadocAction = new
					// OpenExternalBrowserAction(
					// parent.getDisplay(), selectionProvider);
					// selectionProvider
					// .addSelectionChangedListener(openExternalJavadocAction);
					// selectionProvider.setSelection(new
					// StructuredSelection());
					// tbm.add(openExternalJavadocAction);

					final IInputChangedListener inputChangeListener = new IInputChangedListener() {
						public void inputChanged(final Object newInput) {
							backAction.update();
							forwardAction.update();
							if (newInput == null) {
								selectionProvider
										.setSelection(new StructuredSelection());
							} else if (newInput instanceof BrowserInformationControlInput) {
								final BrowserInformationControlInput input = (BrowserInformationControlInput) newInput;
								final Object inputElement = input
										.getInputElement();
								selectionProvider
										.setSelection(new StructuredSelection(
												inputElement));
								final boolean hasInputElement = inputElement != null;
								showInJavadocViewAction
										.setEnabled(hasInputElement);
								openDeclarationAction
										.setEnabled(hasInputElement);
							}
						}
					};
					iControl.addInputChangeListener(inputChangeListener);

					tbm.update(true);

					return iControl;
				} catch (final NoSuchMethodError e) {
					// API changed in 3.4
					return new DefaultInformationControl(parent, EditorsUI
							.getTooltipAffordanceString(),
							new HTMLTextPresenter(true));
				}
			} else {
				return new DefaultInformationControl(parent, EditorsUI
						.getTooltipAffordanceString(), new HTMLTextPresenter(
						true));
			}
		}
	}

	public IInformationControlCreator getHoverControlCreator() {
		if (fHoverControlCreator == null) {
			fHoverControlCreator = new HoverControlCreator(
					getInformationPresenterControlCreator());
		}
		return fHoverControlCreator;
	}

	public static final class HoverControlCreator extends
			AbstractReusableInformationControlCreator {
		IInformationControlCreator fInformationPresenterControlCreator;

		public HoverControlCreator(
				final IInformationControlCreator informationPresenterControlCreator) {
			fInformationPresenterControlCreator = informationPresenterControlCreator;
		}

		@SuppressWarnings("restriction")
		@Override
		protected IInformationControl doCreateInformationControl(
				final Shell parent) {
			if (BrowserInformationControl.isAvailable(parent)) {
				try {
					return new BrowserInformationControl(parent,
							JFaceResources.DIALOG_FONT, EditorsUI
									.getTooltipAffordanceString()) {
						@Override
						public IInformationControlCreator getInformationPresenterControlCreator() {
							return fInformationPresenterControlCreator;
						}
					};
				} catch (final NoSuchMethodError e) {
					// API changed in 3.4
					return new DefaultInformationControl(parent, EditorsUI
							.getTooltipAffordanceString(),
							new HTMLTextPresenter(true));
				}
			} else {
				return new DefaultInformationControl(parent, EditorsUI
						.getTooltipAffordanceString(), new HTMLTextPresenter(
						true));
			}
		}
	}

	public static String getHoverTextForOffset(final int offset,
			final ErlangEditor editor) {
		final ErlTextHover h = new ErlTextHover(null, ErlModelUtils
				.getModule(editor));
		final ITextViewer tv = editor.getViewer();
		final IRegion r = h.getHoverRegion(tv, offset);
		if (r == null) {
			return null;
		}
		return h.getHoverInfo(tv, r);
	}

	public String getHoverInfo(final ITextViewer textViewer,
			final IRegion hoverRegion) {
		final ErlBrowserInformationControlInput input = (ErlBrowserInformationControlInput) getHoverInfo2(
				textViewer, hoverRegion);
		return input == null ? "" : input.getHtml();
	}

	public Object getHoverInfo2(final ITextViewer textViewer,
			final IRegion hoverRegion) {
		return internalGetHoverInfo(fEditor, fModule, textViewer, hoverRegion);
	}

	@SuppressWarnings("restriction")
	private static ErlBrowserInformationControlInput internalGetHoverInfo(
			final ErlangEditor editor, final IErlModule module,
			final ITextViewer textViewer, final IRegion hoverRegion) {
		if (module == null) {
			return null;
		}
		final StringBuffer result = new StringBuffer();
		Object element = null;
		// TODO our model is too coarse, here we need access to expressions
		// try {
		// element = module.getElementAt(hoverRegion.getOffset());
		// } catch (Exception e) {
		// }
		final Collection<IErlImport> fImports = ErlModelUtils
				.getImportsAsList(module);

		final int offset = hoverRegion.getOffset();
		OtpErlangObject r1 = null;
		final int length = hoverRegion.getLength();
		final String debuggerVar = makeDebuggerVariableHover(textViewer,
				offset, length);
		if (debuggerVar.length() > 0) {
			result.append(debuggerVar);
		}
		final String stateDir = ErlideUIPlugin.getDefault().getStateLocation()
				.toString();
		final Backend b = ErlangCore.getBackendManager().getIdeBackend();
		final IErlModel model = ErlangCore.getModel();
		final IErlProject erlProject = module.getProject();
		r1 = ErlideDoc.getDocFromScan(b, offset, stateDir, ErlScanner
				.createScannerModuleName(module), fImports, model.getExternal(
				erlProject, ErlangCore.EXTERNAL_MODULES), model.getPathVars());
		// ErlLogger.debug("getHoverInfo getDocFromScan " + r1);
		final OtpErlangTuple t = (OtpErlangTuple) r1;
		if (Util.isOk(t)) {
			final String docStr = Util.stringValue(t.elementAt(1));
			final OpenResult or = new OpenResult(t.elementAt(2));
			result.append(docStr);
			element = or;
		} else {
			// TODO here we should check like in 'open'
			final OtpErlangObject o0 = t.elementAt(0);
			final OtpErlangObject o1 = t.elementAt(1);
			if (o0 instanceof OtpErlangAtom && o1 instanceof OtpErlangAtom) {
				final OtpErlangAtom a0 = (OtpErlangAtom) o0;
				final OtpErlangAtom a1 = (OtpErlangAtom) o1;
				final String openKind = a0.atomValue();
				if (openKind.equals("error")) {
					return null;
				}
				String definedName = a1.atomValue();
				if (definedName.charAt(0) == '?') {
					definedName = definedName.substring(1);
				}
				// TODO code below should be cleaned up, we should factorize and
				// use same code for content assist, open and hover
				if (openKind.equals("local") || openKind.equals("external")) {
					IErlModule m = null;
					IErlFunction f = null;
					OtpErlangLong arityLong = null;
					if (openKind.equals("local")) {
						arityLong = (OtpErlangLong) t.elementAt(2);
						m = module;
					} else if (openKind.equals("external")) {
						final OtpErlangAtom a2 = (OtpErlangAtom) t.elementAt(2);
						final String mod = definedName;
						definedName = a2.atomValue();
						arityLong = (OtpErlangLong) t.elementAt(3);
						final OtpErlangString s4;
						if (t.elementAt(4) instanceof OtpErlangString) {
							s4 = (OtpErlangString) t.elementAt(4);
						} else {
							final String msg = "unrecognized value: %s, expected a string instead of %s";
							ErlLogger.warn(msg, t, t.elementAt(4));
							return null;
						}
						final String path = Util.stringValue(s4);
						IResource r = null;
						try {
							r = ErlModelUtils.openExternalModule(mod, path,
									module.getResource().getProject());
						} catch (final CoreException e2) {
						}
						if (!(r instanceof IFile)) {
							return null;
						}
						final IFile file = (IFile) r;
						m = ErlModelUtils.getModule(file);
					}
					int arity = -1;
					try {
						if (arityLong != null) {
							arity = arityLong.intValue();
						}
					} catch (final OtpErlangRangeException e) {
					}
					final ErlangFunction erlangFunction = new ErlangFunction(
							definedName, arity);
					if (m == null) {
						return null;
					}
					try {
						m.open(null);
						f = ErlModelUtils.findFunction(m, erlangFunction);
						element = f;
					} catch (final ErlModelException e) {
					}
					if (f == null) {
						return null;
					}
					final String comment = f.getComment();
					if (comment == null) {
						return null;
					}
					result.append(comment);
				} else {
					final IErlElement.Kind kindToFind = openKind
							.equals("record") ? IErlElement.Kind.RECORD_DEF
							: IErlElement.Kind.MACRO_DEF;
					final IErlProject project = module.getProject();
					final IProject proj = project == null ? null
							: (IProject) project.getResource();
					definedName = OpenResult.removeQuestionMark(a1.toString());
					final String externalIncludes = model.getExternal(
							erlProject, ErlangCore.EXTERNAL_INCLUDES);
					IErlPreprocessorDef pd = ErlModelUtils.findPreprocessorDef(
							b, proj, module, definedName, kindToFind,
							externalIncludes);
					if (pd == null) {
						pd = ErlModelUtils.findPreprocessorDef(b, proj, module,
								ErlideUtil.unquote(definedName), kindToFind,
								externalIncludes);
					}
					if (pd != null) {
						element = pd;
						result.append(pd.getExtra());
					}
				}
			}
		}
		if (result.length() > 0) {
			HTMLPrinter.insertPageProlog(result, 0, fgStyleSheet);
			HTMLPrinter.addPageEpilog(result);
		}
		// TODO set element
		return new ErlBrowserInformationControlInput(null, element, result
				.toString(), 20);
	}

	/**
	 * Action to go back to the previous input in the hover control.
	 */
	@SuppressWarnings("restriction")
	private static final class BackAction extends Action {
		private final BrowserInformationControl fInfoControl;

		public BackAction(final BrowserInformationControl infoControl) {
			fInfoControl = infoControl;
			setText("Previous");
			final ISharedImages images = PlatformUI.getWorkbench()
					.getSharedImages();
			setImageDescriptor(images
					.getImageDescriptor(ISharedImages.IMG_TOOL_BACK));
			setDisabledImageDescriptor(images
					.getImageDescriptor(ISharedImages.IMG_TOOL_BACK_DISABLED));

			update();
		}

		@Override
		public void run() {
			final BrowserInformationControlInput previous = (BrowserInformationControlInput) fInfoControl
					.getInput().getPrevious();
			if (previous != null) {
				fInfoControl.setInput(previous);
			}
		}

		public void update() {
			final BrowserInformationControlInput current = fInfoControl
					.getInput();

			if (current != null && current.getPrevious() != null) {
				final BrowserInput previous = current.getPrevious();
				setToolTipText(String.format("Go back to %s", previous
						.getInputName()));
				setEnabled(true);
			} else {
				setToolTipText("");
				setEnabled(false);
			}
		}
	}

	/**
	 * Action to go forward to the next input in the hover control.
	 */
	@SuppressWarnings("restriction")
	private static final class ForwardAction extends Action {
		private final BrowserInformationControl fInfoControl;

		public ForwardAction(final BrowserInformationControl infoControl) {
			fInfoControl = infoControl;
			setText("Next");
			final ISharedImages images = PlatformUI.getWorkbench()
					.getSharedImages();
			setImageDescriptor(images
					.getImageDescriptor(ISharedImages.IMG_TOOL_FORWARD));
			setDisabledImageDescriptor(images
					.getImageDescriptor(ISharedImages.IMG_TOOL_FORWARD_DISABLED));

			update();
		}

		@Override
		public void run() {
			final BrowserInformationControlInput next = (BrowserInformationControlInput) fInfoControl
					.getInput().getNext();
			if (next != null) {
				fInfoControl.setInput(next);
			}
		}

		public void update() {
			final BrowserInformationControlInput current = fInfoControl
					.getInput();

			if (current != null && current.getNext() != null) {
				setToolTipText(String.format("Go to next %s", current.getNext()
						.getInputName()));
				setEnabled(true);
			} else {
				setToolTipText("");
				setEnabled(false);
			}
		}
	}

	/**
	 * Action that shows the current hover contents in the Javadoc view.
	 */
	@SuppressWarnings("restriction")
	private static final class ShowInJavadocViewAction extends Action {
		private final BrowserInformationControl fInfoControl;

		public ShowInJavadocViewAction(
				final BrowserInformationControl infoControl) {
			fInfoControl = infoControl;
			setText("Show in eDoc view");
			setImageDescriptor(ErlideUIPluginImages.DESC_OBJS_EDOCTAG);
		}

		/*
		 * @see org.eclipse.jface.action.Action#run()
		 */
		@Override
		public void run() {
			final ErlBrowserInformationControlInput infoInput = (ErlBrowserInformationControlInput) fInfoControl
					.getInput();
			fInfoControl.notifyDelayedInputChange(null);
			fInfoControl.dispose();
			try {
				final EdocView view = (EdocView) ErlideUIPlugin.getActivePage()
						.showView(EdocView.ID);
				// TODO view.setInput(infoInput);
				view.setText(infoInput.getHtml());
			} catch (final PartInitException e) {
				ErlLogger.error(e);
			}
		}
	}

	/**
	 * Action that opens the current hover input element.
	 * 
	 * @since 3.4
	 */
	@SuppressWarnings("restriction")
	private static final class OpenDeclarationAction extends Action {
		private final BrowserInformationControl fInfoControl;

		public OpenDeclarationAction(final BrowserInformationControl infoControl) {
			fInfoControl = infoControl;
			setText("Open declaration");
			ErlideUIPluginImages.setLocalImageDescriptors(this,
					"goto_input.gif");
		}

		/*
		 * @see org.eclipse.jface.action.Action#run()
		 */
		@Override
		public void run() {
			final BrowserInformationControlInput infoInput = fInfoControl
					.getInput();
			fInfoControl.notifyDelayedInputChange(null);
			fInfoControl.dispose();
			// try {
			// // FIXME: add hover location to editor navigation history?
			try {
				final Object element = infoInput.getInputElement();
				if (element instanceof IErlElement) {
					EditorUtility.openElementInEditor(element, true);
				} else if (element instanceof OpenResult) {
					final OpenResult or = (OpenResult) element;
					try {
						OpenAction.openOpenResult(null, null, ErlangCore
								.getBackendManager().getIdeBackend(), -1, null,
								or);
					} catch (final Exception e) {
						e.printStackTrace();
					}
				}
			} catch (final PartInitException e) {
				e.printStackTrace();
			} catch (final ErlModelException e) {
				e.printStackTrace();
			}
			// } catch (PartInitException e) {
			// ErlLogger.error(e);
			// } catch (ErlModelException e) {
			// ErlLogger.error(e);
			// }
		}
	}

}
