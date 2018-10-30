namespace CodeGeneration.UI
{
    partial class UcSettings
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

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.cbIncludeNonDboSchemaInNamespace = new System.Windows.Forms.CheckBox();
            this.cbUseOptionTypes = new System.Windows.Forms.CheckBox();
            this.txtTargetNamespace = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.txtTargetProjectName = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.SuspendLayout();
            // 
            // cbIncludeNonDboSchemaInNamespace
            // 
            this.cbIncludeNonDboSchemaInNamespace.AutoSize = true;
            this.cbIncludeNonDboSchemaInNamespace.Location = new System.Drawing.Point(108, 31);
            this.cbIncludeNonDboSchemaInNamespace.Name = "cbIncludeNonDboSchemaInNamespace";
            this.cbIncludeNonDboSchemaInNamespace.Size = new System.Drawing.Size(140, 17);
            this.cbIncludeNonDboSchemaInNamespace.TabIndex = 23;
            this.cbIncludeNonDboSchemaInNamespace.Text = "IncludeNonDboSchema";
            this.toolTip1.SetToolTip(this.cbIncludeNonDboSchemaInNamespace, "Include Non-Dbo Schemas in namespace");
            this.cbIncludeNonDboSchemaInNamespace.UseVisualStyleBackColor = true;
            // 
            // cbUseOptionTypes
            // 
            this.cbUseOptionTypes.AutoSize = true;
            this.cbUseOptionTypes.Location = new System.Drawing.Point(6, 31);
            this.cbUseOptionTypes.Name = "cbUseOptionTypes";
            this.cbUseOptionTypes.Size = new System.Drawing.Size(105, 17);
            this.cbUseOptionTypes.TabIndex = 22;
            this.cbUseOptionTypes.Text = "UseOptionTypes";
            this.cbUseOptionTypes.UseVisualStyleBackColor = true;
            // 
            // txtTargetNamespace
            // 
            this.txtTargetNamespace.Location = new System.Drawing.Point(319, 1);
            this.txtTargetNamespace.Name = "txtTargetNamespace";
            this.txtTargetNamespace.Size = new System.Drawing.Size(100, 20);
            this.txtTargetNamespace.TabIndex = 21;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(214, 4);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(95, 13);
            this.label2.TabIndex = 20;
            this.label2.Text = "TargetNamespace";
            // 
            // txtTargetProjectName
            // 
            this.txtTargetProjectName.Location = new System.Drawing.Point(108, 1);
            this.txtTargetProjectName.Name = "txtTargetProjectName";
            this.txtTargetProjectName.Size = new System.Drawing.Size(100, 20);
            this.txtTargetProjectName.TabIndex = 19;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(3, 4);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(99, 13);
            this.label1.TabIndex = 18;
            this.label1.Text = "TargetProjectName";
            // 
            // UcSettings
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.cbIncludeNonDboSchemaInNamespace);
            this.Controls.Add(this.cbUseOptionTypes);
            this.Controls.Add(this.txtTargetNamespace);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.txtTargetProjectName);
            this.Controls.Add(this.label1);
            this.Name = "UcSettings";
            this.Size = new System.Drawing.Size(764, 276);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.CheckBox cbIncludeNonDboSchemaInNamespace;
        private System.Windows.Forms.CheckBox cbUseOptionTypes;
        private System.Windows.Forms.TextBox txtTargetNamespace;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox txtTargetProjectName;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.ToolTip toolTip1;
    }
}
