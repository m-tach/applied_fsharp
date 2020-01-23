using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;

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
		private Dictionary<string, SharedTypes.SharedTypes.GameServer> servers = new Dictionary<string, SharedTypes.SharedTypes.GameServer>();

		public MainWindow()
		{
			InitializeComponent();
			client = new Client.ClientStuff.Client(new SharedTypes.SharedTypes.AsyncEventQueue<SharedTypes.SharedTypes.Message>());

			client.NewGameServerFoundEvent += Client_NewGameServerFound;
		}

		private void Client_NewGameServerFound(object sender, SharedTypes.SharedTypes.GameServer server)
		{
			servers[server.Address.ToString()] = server;
			SetLobbyGames();
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
		private void SetLobbyGames() => GameServerList.ItemsSource = servers.ToArray().Select(serv => serv.Value).ToList();

		private void BroadcastForGames()
		{
			
		}

		private void Refresh_Click(object sender, RoutedEventArgs e) => BroadcastForGames();

		// Render the game given a state.
		public void RenderGame(SharedTypes.SharedTypes.GameState state)
		{
			int middleY = 180;
			int middleX = 387;

			GameCanvasPlayer1.Y1 = middleY + state.Player1.Position.Y * 10;
			GameCanvasPlayer1.Y2 = middleY + state.Player1.Position.Y * 10 + 80;

			GameCanvasPlayer2.Y1 = middleY + state.Player2.Position.Y * 10;
			GameCanvasPlayer2.Y2 = middleY + state.Player2.Position.Y * 10 + 80;

			GameCanvasBall.Margin = new Thickness(middleX + state.Ball.BallPosition.X * 10, middleY + state.Ball.BallPosition.Y * 10, 0, 0);

			Player1Score.Content = $"P1: {state.Player1.Score} Points";
			Player2Score.Content = $"P2: {state.Player2.Score} Points";
		}

		// When the window has loaded, ping for game hosts and get 'em.
		private void Window_Loaded(object sender, RoutedEventArgs e)
		{
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
			Button btn = e.Source as Button;
			string key = btn.Tag.ToString();

			if (servers.ContainsKey(key)) client.JoinGame(servers[key]);
			else MessageBox.Show("The game host does not exist.");
		}

		// Exit a game screen. Brings you to the lobby.
		private void ExitGameBtn_Click(object sender, RoutedEventArgs e) => SetScreen(ScreenLobby);

		// Listen for keypresses and send those to the Client. Later we will have holding the button loop the direction.
		private void Window_KeyDown(object sender, System.Windows.Input.KeyEventArgs e)
		{
			SharedTypes.SharedTypes.Input input = e.Key switch
			{
				System.Windows.Input.Key.W => SharedTypes.SharedTypes.Input.Up,
				System.Windows.Input.Key.Up => SharedTypes.SharedTypes.Input.Up,
				System.Windows.Input.Key.S => SharedTypes.SharedTypes.Input.Down,
				System.Windows.Input.Key.Down => SharedTypes.SharedTypes.Input.Down,
				System.Windows.Input.Key.Escape => SharedTypes.SharedTypes.Input.Escape,
				_ => null
			};
			if (input != null) client.KeyPressed(input);
		}

		// Host the game. Immediately changes screen, although the host may take a bit of time to become available (network stuff)
		private void CreateGameBtn_Click(object sender, RoutedEventArgs e)
		{
			client.HostGame(GameNameCreateGame.Text);
			SetScreen(ScreenGame);
		}
	}

}