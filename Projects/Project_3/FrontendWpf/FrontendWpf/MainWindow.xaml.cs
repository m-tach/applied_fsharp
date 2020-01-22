using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace FrontendWpf
{
	/// <summary>
	/// Interaction logic for MainWindow.xaml
	/// </summary>
	public partial class MainWindow : Window
	{
		public MainWindow()
		{
			InitializeComponent();
		}

		// Sets the current screen by hiding all screens and showing the screen.
		private void SetScreen(StackPanel screen)
		{
			WindowContent.Children.Cast<UIElement>()
								  .Where(scr => scr is StackPanel)
								  .ToList()
								  .ForEach(scr => scr.Visibility = Visibility.Hidden);

			screen.Visibility = Visibility.Visible;
		}

		private void SetLobbyGames(GameServer[] games)
		{
			GameServerList.ItemsSource = games;
		}

		private void Window_Loaded(object sender, RoutedEventArgs e)
		{
			SetLobbyGames(new GameServer[] {
				new GameServer("Hello World", System.Net.IPAddress.None),
				new GameServer("World Hello", System.Net.IPAddress.None),
				new GameServer("Hello Hello", System.Net.IPAddress.None)
			});
		}

		private void HostGame_Click(object sender, RoutedEventArgs e) => SetScreen(ScreenCreateGame);

		private void ExitCreateGame_Click(object sender, RoutedEventArgs e) => SetScreen(ScreenLobby);

		private void JoinGameBtn_Click(object sender, RoutedEventArgs e)
		{
			System.Diagnostics.Debug.Print(e.Source + ";; " + e.RoutedEvent + ";; " + e.OriginalSource);
		}
	}

	public class GameServer
	{
		public string Name { get; set; }
		public System.Net.IPAddress IPAddress { get; set; }

		public GameServer(string name, System.Net.IPAddress ipAddress)
		{
			Name = name;
			IPAddress = ipAddress;
		}
	}
}
