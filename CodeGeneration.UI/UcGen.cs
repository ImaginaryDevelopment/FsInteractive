using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using static CodeGeneration.GenerateAllTheThings;

namespace CodeGeneration.UI
{
    public partial class UcGen : UserControl
    {
        public TableInput[] Items { get; set; }
        public UcGen()
        {
            InitializeComponent();
        }

        void btnGenerate_Click(object sender, EventArgs e)
        {
            if(this.Items == null || this.Items.Length < 1)
            {
                MessageBox.Show("No items imported");
                return;
            }
            if (cbName.SelectedIndex < 0 || cbName.SelectedText.IsValueString() == false)
            {
                MessageBox.Show("Select the name of the item you wish to import");
                return;
            }
            if (cbType.SelectedIndex < 0 || cbType.SelectedText.IsValueString() == false)
            {
                MessageBox.Show("Select the type of generation you wish to use");
                return;
            }

        }
    }
}
