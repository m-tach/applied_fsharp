using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Threading;

using SharedTypes;

namespace FrontendWpf3
{
	/// <summary>
	/// Interaction logic for MainWindow.xaml
	/// </summary>
	public partial class MainWindow : Window
	{
		// Client F# library
		
		private Client.ClientStuff.Client client;
		// List of servers discovered
		private readonly ObservableCollection<SharedTypes.SharedTypes.GameServer> Servers = new ObservableCollection<SharedTypes.SharedTypes.GameServer>();


		public MainWindow()
		{
			InitializeComponent();
			client = new Client.ClientStuff.Client();
			client.GoToLobbyEvent += Client_GoToLobbyEvent;
			client.LaunchGameEvent += Client_LaunchGameEvent;
			client.NewGameServerFoundEvent += Client_NewGameServerFound;
			client.NewGameStateEvent += Client_NewGameStateEvent;
			client.WaitForStartGameEvent += Client_WaitForStartGameEvent;
		}

		private void Client_WaitForStartGameEvent(object sender, int args)
		{
			Dispatcher.Invoke(() => this.SetScreen(ScreenWaitForPlayers));
		}

		private void Client_NewGameStateEvent(object sender, SharedTypes.SharedTypes.GameState args)
		{
			Dispatcher.BeginInvoke(() => this.RenderGame(args));
		}

		private void Client_LaunchGameEvent(object sender, Microsoft.FSharp.Core.Unit args)
		{
			Dispatcher.Invoke(() => this.SetScreen(ScreenGame));
		}

		private void Client_GoToLobbyEvent(object sender, Microsoft.FSharp.Core.Unit args)
		{
			Dispatcher.Invoke(() => this.SetScreen(ScreenLobby));
		}

		private void Client_NewGameServerFound(object sender, SharedTypes.SharedTypes.GameServer server)
		{
			Dispatcher.Invoke(() => {
				if (!Servers.Any(x => x.Address.Equals(server.Address)))
					Servers.Add(server);
			});
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

		private void BroadcastForGames()
		{
			client.BroadcastRequestServers();
		}

		private void Refresh_Click(object sender, RoutedEventArgs e) => BroadcastForGames();

		private void SinglePlayer_Click(object sender, RoutedEventArgs e) => client.HostGameComputer();

		// Render the game given a state.
		public void RenderGame(SharedTypes.SharedTypes.GameState state)
		{
			double widthScale = GameCanvas.ActualWidth / 20.0;
			double heightScale = GameCanvas.ActualHeight / 20.0;

			double middleY = GameCanvas.ActualHeight / 2.0;
			double middleX = GameCanvas.ActualWidth / 2.0;

			double paddleHeight = 3.0;
			double paddleHeightHalf = paddleHeight / 2.0;

			int ballRadius = 8;

			GameCanvasPlayer1.Y1 = middleY + (state.Player1.Position.Y - paddleHeightHalf) * heightScale;
			GameCanvasPlayer1.Y2 = middleY + (state.Player1.Position.Y + paddleHeightHalf) * heightScale;

			GameCanvasPlayer2.Y1 = middleY + (state.Player2.Position.Y - paddleHeightHalf) * heightScale;
			GameCanvasPlayer2.Y2 = middleY + (state.Player2.Position.Y + paddleHeightHalf) * heightScale;

			GameCanvasBall.Margin = new Thickness(
				middleX + state.Ball.BallPosition.X * widthScale - ballRadius,
				middleY + state.Ball.BallPosition.Y * heightScale - ballRadius,
				0, 0
			);

			Player1Score.Content = $"P1: {state.Player1.Score} Points";
			Player2Score.Content = $"P2: {state.Player2.Score} Points";
		}

		// When the window has loaded, ping for game hosts and get 'em.
		private void Window_Loaded(object sender, RoutedEventArgs e)
		{
			this.DataContext = this;
			this.GameServerList.ItemsSource = Servers;

			BroadcastForGames();
			/*RenderGame(new GameState()
			{
				Ball = new Ball() { BallPosition = new Vector(15, 0), BallDirection = new Vector(1, 0.25f) },
				Player1 = new PlayerData() { Position = new Vector(0, 4), Score = 4 },
				Player2 = new PlayerData() { Position = new Vector(0, 3), Score = 1 }
			});*/
		}

		// Lobby -> Create game
		private void HostGame_Click(object sender, RoutedEventArgs e) => SetScreen(ScreenCreateGame);
		// Create game -> Lobby
		private void ExitCreateGame_Click(object sender, RoutedEventArgs e) => SetScreen(ScreenLobby);

		// Join a game host.
		private void JoinGameBtn_Click(object sender, RoutedEventArgs e)
		{
			Button btn = (Button)sender;
			IPAddress key = (IPAddress)btn.Tag;

			SharedTypes.SharedTypes.GameServer server = Servers.SingleOrDefault(x => x.Address.Equals(key));

			if (server != default(SharedTypes.SharedTypes.GameServer))
			{
				client.JoinGame(server);
			}
			else MessageBox.Show("The game host does not exist.");
		}

		// Exit a game screen. Brings you to the lobby.
		private void ExitGameBtn_Click(object sender, RoutedEventArgs e)
		{
			client.ExitGame();
		}

		// Listen for keypresses and send those to the Client. Later we will have holding the button loop the direction.
		private void Window_KeyDown(object sender, System.Windows.Input.KeyEventArgs e)
		{
			SharedTypes.SharedTypes.Input input = e.Key switch
			{
				System.Windows.Input.Key.W => SharedTypes.SharedTypes.Input.Down,
				System.Windows.Input.Key.Up => SharedTypes.SharedTypes.Input.Down,
				System.Windows.Input.Key.S => SharedTypes.SharedTypes.Input.Up,
				System.Windows.Input.Key.Down => SharedTypes.SharedTypes.Input.Up,
				System.Windows.Input.Key.Escape => SharedTypes.SharedTypes.Input.Escape,
				_ => null
			};
			if (input != null) client.KeyPressed(input);
		}

		// Host the game. Immediately changes screen, although the host may take a bit of time to become available (network stuff)
		private void CreateGameBtn_Click(object sender, RoutedEventArgs e)
		{
			client.HostGame(GameNameCreateGame.Text);
			SetScreen(ScreenWaitForPlayers);
		}
	}

}