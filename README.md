# Dev Launcher

開発環境を管理するためのCLIランチャーアプリケーション。複数のプロジェクトを登録して一括起動し、TUIでログをリアルタイム監視できます。

## インストール

### Homebrew (macOS)

```bash
brew tap ryokatsuse/tap
brew install dev-launcher
```

### ソースからビルド

```bash
cabal build
cabal install
```

## 使い方

### プロジェクトを登録

```bash
# 基本形式
dev-launcher add <名前> <パス> <起動コマンド> [ポート]

# 例
dev-launcher add my-app ~/projects/my-app "npm run dev" 3000
dev-launcher add storybook ~/projects/my-app "npm run storybook" 6006
```

### 登録済みプロジェクトを確認

```bash
dev-launcher list
```

### プロジェクトを起動

```bash
# 単一プロジェクト
dev-launcher start my-app

# 複数プロジェクトを同時起動（ログがプレフィックス付きで表示）
dev-launcher start-all my-app storybook
```

### TUIモードで起動（推奨）

複数プロジェクトを分割ペインでログ監視しながら起動できます。

```bash
dev-launcher tui my-app storybook
```

**TUIキー操作:**
- `q` - 終了（全プロセスを停止）
- `Tab` - ペイン切り替え
- `↑/↓` - ログのスクロール

### プロジェクトを編集

```bash
dev-launcher edit <名前> <新しいパス> <新しいコマンド> [新しいポート]
```

### プロジェクトを削除

```bash
dev-launcher remove <名前>
```

### ヘルプ

```bash
dev-launcher help
```

## 設定ファイル

プロジェクト情報は以下に保存されます:

```
~/.config/dev-launcher/projects.yaml
```

## 開発

### ビルド

```bash
cabal build
```

### 実行

```bash
cabal run dev-launcher -- <コマンド>
```

### フォーマット

```bash
fourmolu -i app/**/*.hs
```

### リント

```bash
hlint app/
```

## ライセンス

MIT
