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

		// Shorthand, in case we need to do more in the future.
		// Sets the list of games to show on the lobby list.
		private void SetLobbyGames(GameServer[] games) => GameServerList.ItemsSource = games;

		// Render the game given a state.
		public void RenderGame(GameState state)
		{
			int middleY = 180;
			int middleX = 387;

			GameCanvasPlayer1.Y1 = middleY + state.Player1.Position.Y * 10;
			GameCanvasPlayer1.Y2 = middleY + state.Player1.Position.Y * 10 + 80;

			GameCanvasPlayer2.Y1 = middleY + state.Player2.Position.Y * 10;
			GameCanvasPlayer2.Y2 = middleY + state.Player2.Position.Y * 10 + 80;

			GameCanvasBall.Margin = new Thickness(middleX + state.Ball.BallPosition.X * 10, middleY + state.Ball.BallPosition.Y * 10, 0, 0);
		}

		// When the window has loaded, ping for game hosts and get 'em.
		private void Window_Loaded(object sender, RoutedEventArgs e)
		{
			SetLobbyGames(new GameServer[] {
				new GameServer("Hello World", System.Net.IPAddress.None),
				new GameServer("World Hello", System.Net.IPAddress.None),
				new GameServer("Hello Hello", System.Net.IPAddress.None)
			});
			RenderGame(new GameState()
			{
				Ball = new Ball() { BallPosition = new Vector(15, 0), BallDirection = new Vector(1, 0.25f) },
				Player1 = new PlayerData() { Position = new Vector(0, 4), Score = 4 },
				Player2 = new PlayerData() { Position = new Vector(0, 3), Score = 1 }
			});
		}

		// Lobby -> Create game
		private void HostGame_Click(object sender, RoutedEventArgs e) => SetScreen(ScreenCreateGame);
		// Create game -> Lobby
		private void ExitCreateGame_Click(object sender, RoutedEventArgs e) => SetScreen(ScreenLobby);

		// Join a game host. Currently this prints the dummy IP address and switches the view to the game screen.
		private void JoinGameBtn_Click(object sender, RoutedEventArgs e)
		{
			Button btn = e.Source as Button;
			System.Diagnostics.Debug.Print(btn.Tag + "");

			SetScreen(ScreenGame);
		}

		// Exit a game screen. Brings you to the lobby.
		private void ExitGameBtn_Click(object sender, RoutedEventArgs e) => SetScreen(ScreenLobby);
	}

	// Placeholder, until we tie together the FS modules and this.
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

	public struct Vector
	{
		public float X;
		public float Y;
		public Vector(float x, float y) { X = x; Y = y; }
	}

	public struct PlayerData
	{
		public Vector Position;
		public int Score;
	}

	public struct Ball
	{
		public Vector BallPosition;
		public Vector BallDirection;
	}

	public struct GameState
	{
		public Ball Ball;
		public PlayerData Player1;
		public PlayerData Player2;
	}
}
