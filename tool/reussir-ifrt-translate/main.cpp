//===----------------------------------------------------------------------===//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//===----------------------------------------------------------------------===//

// Compiled from upstream's IFRT translation driver by the Bazel bridge.
int reussirIfrtTranslateMain(int argc, char **argv);

int main(int argc, char **argv) { return reussirIfrtTranslateMain(argc, argv); }
