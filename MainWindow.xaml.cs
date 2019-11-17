using System;
using System.Runtime.InteropServices;
using System.Windows;
using System.Windows.Controls;
using FS = ClassLibraryFSharp;

namespace WPFInNetCore
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern bool Beep(int frequency, int duration);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        static extern bool AllocConsole();

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        static extern bool FreeConsole();
        public MainWindow()
        {
            InitializeComponent();
            AllocConsole();

            FS.SimplexMethod.MatrixA = new float [,] { { 2.5f, 0f }, { 0f, 1f} };
            FS.SimplexMethod.Znak = new string [] { "=", "=", "=", "=" };
            FS.SimplexMethod.VecB =  new float[]{200f,1000f,900f,5000f};
            FS.Logical.StartProgram();
            Console.WriteLine();
            Console.WriteLine("______________Новый метод______________");
            Console.WriteLine();
            FS.SimplexMethod.MatrixA = new float[,] {{1f, 0f, 1f, 2.5f}, {0f, 1f, 1f, 6f}};
            FS.SimplexMethod.Znak = new string[] {"<", "<", ">", "="};
            FS.Logical.StartProgram();



        }
    }
}
