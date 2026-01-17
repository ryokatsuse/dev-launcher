# Dev Launcher

開発環境を管理するためのCLIランチャーアプリケーション。

## 機能（予定）

- 複数プロジェクトの起動コマンド登録
- 選択式でのプロジェクト起動
- ポート使用状況の確認
- プロセスの停止管理

## ビルド

```bash
cabal build
```

## 実行

```bash
cabal run dev-launcher
```

## 開発

### フォーマット

```bash
fourmolu -i app/**/*.hs
```

### リント

```bash
hlint app/
```
