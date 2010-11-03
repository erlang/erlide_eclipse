package org.erlide.eunit.ui.launch;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.SelectionDialog;

/**
 * Browser for selecting projects, modules or files.
 * 
 * @author Aleksandra Lipiec
 *
 */
public class ItemBrowser{
	
	private Text text;
	private Button button;
	private SelectionDialog dialog;
	
	public ItemBrowser(Composite comp, int style, SelectionDialog dial){
		
		GridData gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
		gd.widthHint = 500;
		
		text = new Text(comp, style);               
        text.setLayoutData(gd);
        //text.setEditable(false);
        
        button = new Button(comp, SWT.CENTER | SWT.PUSH);
		button.setText("Browse");
		button.addSelectionListener(new SelectionAdapter() {
			
			public void widgetSelected(SelectionEvent e) {
				
				dialog.open();
				text.setText(dialog.getResult().toString());
				//TODO: better serving results
			}
			
		});		
		
	}
	
	public void addModifyListener(ModifyListener ml){
		text.addModifyListener(ml);
	}
	
	public void setFiledLength(int size){
		((GridData) text.getLayoutData()).widthHint = size;
	}
	
	public String getText(){
		return text.getText();
	}
	
	public void setText(String info){
		text.setText(info);
	}
	
	public GridData getTextGridData(){
		return (GridData)text.getLayoutData();
	}
	
}
