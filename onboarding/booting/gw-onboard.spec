# -*- mode: python ; coding: utf-8 -*-


a = Analysis(
    ['gw-onboard.py'],
    pathex=[],
    binaries=[],
    datas=[],
    hiddenimports=['requests', 'nacl.bindings', 'embit.util.secp256k1', '_cffi_backend', 'bitstring.bitstore_bitarray', 'bitstring.bitstore_bitarray_helpers', 'bitstring.bitstore_common_helpers', 'bitstring.bitstore_tibs', 'bitstring.bitstore_tibs_helpers', 'bitarray', 'bitarray._bitarray'],
    hookspath=[],
    hooksconfig={},
    runtime_hooks=[],
    excludes=[],
    noarchive=False,
    optimize=0,
)
pyz = PYZ(a.pure)

exe = EXE(
    pyz,
    a.scripts,
    a.binaries,
    a.datas,
    [],
    name='gw-onboard',
    debug=False,
    bootloader_ignore_signals=False,
    strip=False,
    upx=True,
    upx_exclude=[],
    runtime_tmpdir=None,
    console=True,
    disable_windowed_traceback=False,
    argv_emulation=False,
    target_arch=None,
    codesign_identity=None,
    entitlements_file=None,
)
