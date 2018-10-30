namespace CodeGeneration.UI
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.bsTables = new System.Windows.Forms.BindingSource(this.components);
            this.statusStrip1 = new System.Windows.Forms.StatusStrip();
            this.tsStatus = new System.Windows.Forms.ToolStripStatusLabel();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tbSettings = new System.Windows.Forms.TabPage();
            this.ucSettings1 = new CodeGeneration.UI.UcSettings();
            this.tbJson = new System.Windows.Forms.TabPage();
            this.ucJsonImport1 = new CodeGeneration.UI.UcJsonImport();
            this.tbGen = new System.Windows.Forms.TabPage();
            ((System.ComponentModel.ISupportInitialize)(this.bsTables)).BeginInit();
            this.statusStrip1.SuspendLayout();
            this.tabControl1.SuspendLayout();
            this.tbSettings.SuspendLayout();
            this.tbJson.SuspendLayout();
            this.SuspendLayout();
            // 
            // statusStrip1
            // 
            this.statusStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.tsStatus});
            this.statusStrip1.Location = new System.Drawing.Point(0, 428);
            this.statusStrip1.Name = "statusStrip1";
            this.statusStrip1.Size = new System.Drawing.Size(800, 22);
            this.statusStrip1.TabIndex = 7;
            this.statusStrip1.Text = "statusStrip1";
            // 
            // tsStatus
            // 
            this.tsStatus.Name = "tsStatus";
            this.tsStatus.Size = new System.Drawing.Size(39, 17);
            this.tsStatus.Text = "Ready";
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tbSettings);
            this.tabControl1.Controls.Add(this.tbJson);
            this.tabControl1.Controls.Add(this.tbGen);
            this.tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabControl1.Location = new System.Drawing.Point(0, 0);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(800, 428);
            this.tabControl1.TabIndex = 8;
            // 
            // tbSettings
            // 
            this.tbSettings.Controls.Add(this.ucSettings1);
            this.tbSettings.Location = new System.Drawing.Point(4, 22);
            this.tbSettings.Name = "tbSettings";
            this.tbSettings.Padding = new System.Windows.Forms.Padding(3);
            this.tbSettings.Size = new System.Drawing.Size(792, 402);
            this.tbSettings.TabIndex = 1;
            this.tbSettings.Text = "Settings";
            this.tbSettings.UseVisualStyleBackColor = true;
            // 
            // ucSettings1
            // 
            this.ucSettings1.Location = new System.Drawing.Point(6, 6);
            this.ucSettings1.Name = "ucSettings1";
            this.ucSettings1.Size = new System.Drawing.Size(764, 276);
            this.ucSettings1.TabIndex = 0;
            // 
            // tbJson
            // 
            this.tbJson.Controls.Add(this.ucJsonImport1);
            this.tbJson.Location = new System.Drawing.Point(4, 22);
            this.tbJson.Name = "tbJson";
            this.tbJson.Padding = new System.Windows.Forms.Padding(3);
            this.tbJson.Size = new System.Drawing.Size(792, 402);
            this.tbJson.TabIndex = 0;
            this.tbJson.Text = "JSON";
            this.tbJson.ToolTipText = "Input data models by JSON text";
            this.tbJson.UseVisualStyleBackColor = true;
            // 
            // ucJsonImport1
            // 
            this.ucJsonImport1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ucJsonImport1.Location = new System.Drawing.Point(3, 3);
            this.ucJsonImport1.Name = "ucJsonImport1";
            this.ucJsonImport1.Size = new System.Drawing.Size(786, 396);
            this.ucJsonImport1.TabIndex = 0;
            // 
            // tbGen
            // 
            this.tbGen.Location = new System.Drawing.Point(4, 22);
            this.tbGen.Name = "tbGen";
            this.tbGen.Padding = new System.Windows.Forms.Padding(3);
            this.tbGen.Size = new System.Drawing.Size(792, 402);
            this.tbGen.TabIndex = 2;
            this.tbGen.Text = "Generate";
            this.tbGen.UseVisualStyleBackColor = true;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(800, 450);
            this.Controls.Add(this.tabControl1);
            this.Controls.Add(this.statusStrip1);
            this.Name = "Form1";
            this.Text = "FrmCodegen";
            ((System.ComponentModel.ISupportInitialize)(this.bsTables)).EndInit();
            this.statusStrip1.ResumeLayout(false);
            this.statusStrip1.PerformLayout();
            this.tabControl1.ResumeLayout(false);
            this.tbSettings.ResumeLayout(false);
            this.tbJson.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion
        private System.Windows.Forms.ToolTip toolTip1;
        private System.Windows.Forms.BindingSource bsTables;
        private System.Windows.Forms.StatusStrip statusStrip1;
        private System.Windows.Forms.ToolStripStatusLabel tsStatus;
        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tbSettings;
        private System.Windows.Forms.TabPage tbJson;
        private UcJsonImport ucJsonImport1;
        private UcSettings ucSettings1;
        private System.Windows.Forms.TabPage tbGen;
    }
}

